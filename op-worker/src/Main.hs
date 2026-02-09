{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE QuasiQuotes    #-}

module Main where

import           Control.Concurrent.Async.Every             (every)
import qualified Data.Aeson                                 as Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy                       as LBS
import           Data.ByteString.UTF8                       as BSU
import qualified Data.Pool                                  as Pool
import           Data.String.Interpolate                    (i, __i'L)
import           Data.Typeable                              (typeOf)
import           GHC.Generics
import           Hasql.Connection                           (Connection,
                                                             acquire)
import qualified Hasql.Connection.Settings                  as ConnectionSetting
import qualified Hasql.Notifications                        as Notifications
import qualified Hasql.Session                              as Hasql
import qualified Op.Db                                      as Db
import           RIO
import           System.Environment                         (lookupEnv)
import           System.Exit                                (die)
import           System.Posix.Signals                       (Handler (..),
                                                             installHandler,
                                                             sigINT)
import           UnliftIO.Concurrent                        (forkIO)

import qualified Op.Worker.Job                              as Job
import qualified Op.Worker.JobLogistics                     as JobLogistics
import qualified Op.Worker.Jobs.Debug                       as DebugJob
import qualified Op.Worker.Jobs.ProcessAwsSnsWebhookMessage as ProcessAwsSnsWebhookMessage
import qualified Op.Worker.Jobs.SendEmail                   as SendEmail

failedAttemptsLimit :: Int32
failedAttemptsLimit = 5

microsecondsBetweenJobQueuePoll :: Int
microsecondsBetweenJobQueuePoll = 10 * 1000 * 1000

main :: IO ()
main = do
  withLogFunction $ \logFunc -> do
    connectionPool <- do
      dbSettings <- getDbConnectionSettings >>= either die pure
      Db.createPool dbSettings

    smtpConfig <- getSmtpSettings >>= either die pure

    -- Initiate a shard state that keeps track of how many parallel jobs are
    -- running and if the worker is shutting down
    sharedWorkerState <- JobLogistics.initSharedWorkerState

    -- Tell the worker to stop accepting new jobs on sigINT
    void $ installHandler sigINT (Catch (JobLogistics.initiateShutdown sharedWorkerState)) Nothing

    listenConnection <- do
      -- We need a dedicated DB connection for listening for pg_notify. It can't go
      -- through PG Bouncer or connection pools or the connection will drop while
      -- we're listening and we won't get any notifications
      listenDbSettings <- getListenDbConnectionSettings >>= either die pure
      acquire listenDbSettings >>= either (die . show) pure

    Notifications.listen listenConnection (Notifications.toPgIdentifier "new_worker_job")

    let workerEnv = WorkerEnv{..}
    void $ forkIO $ Notifications.waitForNotifications (handler workerEnv) listenConnection

    runRIO workerEnv do
      logInfo "Ready to process jobs"

      -- poll the job queue every 10 seconds (with 0 time until the first check)
      void $ liftIO $ every microsecondsBetweenJobQueuePoll (Just 0) do
        forkIO do
          runRIO workerEnv do
            checkJobQueue

      -- block here on shutdown until no more jobs are in progress
      shutDownCircumstances <- JobLogistics.awaitShutdown sharedWorkerState

      case shutDownCircumstances of
        JobLogistics.GracefulShutdown -> logInfo "Gracefully shut down after all in progress jobs were finished"
        JobLogistics.ForcedShutdown -> logWarn "Forced shutdown, some jobs might have been in progress"
    where
      handler workerEnv _channel payload = do
        void $ forkIO do
          runRIO workerEnv do
            case Aeson.decode (LBS.fromStrict payload) of
              Nothing                                      -> do
                logError [i|Unexpected payload in db 'new_worker_job' notification: #{payload}|]
              Just (DbNotification microsecondsUntilRunAt) -> do
                -- just rely on the polling if the run_at is too far into the future
                when (microsecondsUntilRunAt < 2 * microsecondsBetweenJobQueuePoll) do
                  when (microsecondsUntilRunAt > 0) do
                    threadDelay microsecondsUntilRunAt

                  checkJobQueue

withLogFunction :: MonadUnliftIO m => (LogFunc -> m a) -> m a
withLogFunction action = do
  logLevel <- getLogLevelFromEnv
  logOptions <- setLogUseTime True
                  . setLogUseLoc True
                  . setLogMinLevel logLevel
                  <$> logOptionsHandle stderr True
  withLogFunc logOptions action
  where
    getLogLevelFromEnv = do
      mLogLevel <- liftIO $ lookupEnv "LOG_LEVEL" <&> fmap readMaybe
      case mLogLevel of
        Nothing              -> pure LevelInfo
        Just Nothing         -> error "Couldn't parse LOG_LEVEL argument"
        Just (Just logLevel) -> pure logLevel

checkJobQueue :: RIO WorkerEnv ()
checkJobQueue = do
  sharedWorkerState <- asks sharedWorkerState

  mCheckAgain <- do
    JobLogistics.runLimitedParallelJobs sharedWorkerState do
      claimJobWithTransaction \workerJob -> do
        Job.processJob workerJob

  let checkAgain = fromMaybe False mCheckAgain

  when checkAgain do
    checkJobQueue

claimJobWithTransaction ::
  (WorkerJob -> Job.Job WorkerEnv ())
  -> RIO WorkerEnv Bool
claimJobWithTransaction processJob' = do
  env <- ask
  Db.withDbConnection env \connection -> do
    runRIO env do
      bracket
        do Db.beginTransactionOr connection handleDbError
        do const do Db.commitTransactionOr connection handleDbError
        \_ -> do
          mJob <- Db.queryDbOr' connection handleDbError (Hasql.statement () checkJobQueueStatement)

          case mJob of
            Nothing -> do
              logDebug [i|No more jobs, going back to sleep|]
              pure False -- Don't check the queue for more jobs

            Just (jobId, failedAttempts, rawJobDefinition) -> do
              -- Do check the queue for more jobs (Run everything below and then return True)
              True <$ do
                case Aeson.fromJSON rawJobDefinition of
                  Aeson.Error err -> do
                    logError [i|jobId: #{jobId}, could not be parsed: #{err}|]
                    Db.queryDbOr' connection handleDbError (Hasql.statement jobId moveToFailedJobStatement)

                  Aeson.Success (workerJob :: WorkerJob) -> do
                    let onErr exception = pure $ Left $ Job.RetryableError
                            [i|
                            Unexpected exception when processing job: #{exception}

                            Job context: #{Aeson.encode workerJob}
                            |]

                    eResult <- handleAny onErr do
                      Job.runJob env do
                        logInfo
                          [__i'L|
                          Starting to process #{workerJobName workerJob}...
                          jobId: #{jobId}
                          |]

                        processJob' workerJob

                    case eResult of
                      Right () -> do
                        logInfo
                          [__i'L|
                          Done processing #{workerJobName workerJob}
                          jobId: #{jobId}
                          |]

                        Db.queryDbOr' connection handleDbError (Hasql.statement jobId moveToCompletedJobStatement)

                      Left err ->
                        case err of
                          Job.NonRetryableError message -> do
                            logError message
                            Job.cleanUpJobAfterGivingUp workerJob
                            Db.queryDbOr' connection handleDbError (Hasql.statement jobId moveToFailedJobStatement)

                          Job.RetryableError message | failedAttempts >= failedAttemptsLimit - 1 -> do
                            logError
                              [__i'L|
                              Giving up after #{failedAttemptsLimit} failed attempts

                              Error: #{textDisplay message}
                              |]

                            Job.cleanUpJobAfterGivingUp workerJob
                            Db.queryDbOr' connection handleDbError (Hasql.statement jobId moveToFailedJobStatement)

                          Job.RetryableError message -> do
                            logError message
                            Db.queryDbOr' connection handleDbError (Hasql.statement jobId returnJobToQueueStatement)
  where
    checkJobQueueStatement =
      [Db.maybeStatement|
        with jobs as (
          delete from job_queue.queued_worker_jobs
          where id = (
            select id
            from job_queue.queued_worker_jobs
            where run_at <= now()
            order by run_at asc, id
            limit 1
            for update skip locked
          )
          returning *
        )

        insert into job_queue.in_progress_worker_jobs (
          id,
          run_at,
          failed_attempts,
          definition
        )
        select
          id,
          run_at,
          failed_attempts,
          definition
        from jobs
        returning
          id::uuid,
          failed_attempts::int,
          definition::jsonb
      |]

    moveToCompletedJobStatement =
      [Db.resultlessStatement|
        with
          jobs as (
            delete from job_queue.in_progress_worker_jobs
            where id = $1::uuid
            returning *
          )

        insert into job_queue.completed_worker_jobs (
          id,
          run_at,
          picked_up_at,
          failed_attempts,
          definition
        )
        select
          id,
          run_at,
          picked_up_at,
          failed_attempts,
          definition
        from jobs
      |]

    moveToFailedJobStatement =
      [Db.resultlessStatement|
        with
          jobs as (
            delete from job_queue.in_progress_worker_jobs
            where id = $1::uuid
            returning *
          )

        insert into job_queue.failed_worker_jobs (
          id,
          run_at,
          picked_up_at,
          failed_attempts,
          definition
        )
        select
          id,
          run_at,
          picked_up_at,
          failed_attempts + 1,
          definition
        from jobs
      |]

    returnJobToQueueStatement =
      [Db.resultlessStatement|
        with
          jobs as (
            delete from job_queue.in_progress_worker_jobs
            where id = $1::uuid
            returning *
          )

        insert into job_queue.queued_worker_jobs (
          id,
          run_at,
          failed_attempts,
          definition
        )
        select
          id,
          run_at + make_interval(secs => 2 ^ failed_attempts),
          failed_attempts + 1,
          definition
        from jobs
      |]



getListenDbConnectionSettings :: IO (Either String ConnectionSetting.Settings)
getListenDbConnectionSettings = do
    mHost <- fmap BSU.fromString <$> lookupEnv "LISTEN_DB_HOST"
    mPort <- lookupEnv "LISTEN_DB_PORT"
    pure do
      host <- maybeToEither "Error: Missing env variable LISTEN_DB_HOST" mHost
      port :: Int <- maybeToEither "Error: Missing env variable LISTEN_DB_PORT" mPort >>= maybeToEither "Error: Couldn't parse port from LISTEN_DB_PORT" . readMaybe

      let connectionString = [i|host=#{host} dbname=events user=postgres password=postgres port=#{port}|]
      pure $ ConnectionSetting.connectionString connectionString

getDbConnectionSettings :: IO (Either String ConnectionSetting.Settings)
getDbConnectionSettings = do
    mHost <- fmap BSU.fromString <$> lookupEnv "DB_HOST"
    mPort <- lookupEnv "DB_PORT"
    pure do
      host <- maybeToEither "Error: Missing env variable DB_HOST" mHost
      port :: Int <- maybeToEither "Error: Missing env variable DB_PORT" mPort >>= maybeToEither "Error: Couldn't parse port from DB_PORT" . readMaybe

      let connectionString = [i|host=#{host} dbname=events user=postgres password=postgres port=#{port}|]
      pure $ ConnectionSetting.connectionString connectionString


getSmtpSettings :: IO (Either String SendEmail.SmtpConfig)
getSmtpSettings = do
    mServer <- lookupEnv "SMTP_SERVER"
    mPort <- lookupEnv "SMTP_PORT"
    mLogin <- lookupEnv "SMTP_LOGIN"
    mPassword <- lookupEnv "SMTP_PASSWORD"
    pure do
      server <- maybeToEither "Error: Missing env variable SMTP_SERVER" mServer
      port <- maybeToEither "Error: Missing env variable SMTP_PORT" mPort >>= maybeToEither "Error: Couldn't parse port from SMTP_PORT" . readMaybe
      login <- maybeToEither "Error: Missing env variable SMTP_LOGIN" mLogin
      password <- maybeToEither "Error: Missing env variable SMTP_PASSWORD" mPassword

      pure SendEmail.SmtpConfig{..}


maybeToEither :: err -> Maybe a -> Either err a
maybeToEither _ (Just a)  = Right a
maybeToEither err Nothing = Left err

data WorkerEnv = WorkerEnv
  { connectionPool    :: Pool.Pool Connection
  -- , jobId          :: UUID.UUID
  , smtpConfig        :: SendEmail.SmtpConfig
  , logFunc           :: LogFunc
  , sharedWorkerState :: JobLogistics.SharedWorkerState
  }

instance HasLogFunc WorkerEnv where
  logFuncL = lens logFunc (\env f -> env { logFunc = f })

instance SendEmail.HasSmtpConfig WorkerEnv where
  getSmtpConfig = smtpConfig

instance Db.HasDbConnection WorkerEnv where
  withDbConnection WorkerEnv{connectionPool} =
    Db.withDbConnection connectionPool


data WorkerJob
  = SendEmail SendEmail.SendEmailJob
  | ProcessAwsSnsWebhookMessage ProcessAwsSnsWebhookMessage.ProcessAwsSnsWebhookMessageJob
  | Debug DebugJob.DebugJob
  deriving (Generic, Show)

instance Job.JobDefinition WorkerEnv WorkerJob where
  processJob wj =
    case wj of
      SendEmail sendEmail                                     -> Job.processJob sendEmail
      Debug debugJob                                          -> Job.processJob debugJob
      ProcessAwsSnsWebhookMessage processAwsSnsWebhookMessage -> Job.processJob processAwsSnsWebhookMessage

  cleanUpJobAfterGivingUp wj =
    case wj of
      SendEmail sendEmail                                     -> Job.cleanUpJobAfterGivingUp sendEmail
      Debug debugJob                                          -> Job.cleanUpJobAfterGivingUp debugJob
      ProcessAwsSnsWebhookMessage processAwsSnsWebhookMessage -> Job.cleanUpJobAfterGivingUp processAwsSnsWebhookMessage

instance Aeson.ToJSON WorkerJob where
  toJSON = Aeson.genericToJSON defaultOptions
    { sumEncoding = TaggedObject "type" "payload"
    }

instance Aeson.FromJSON WorkerJob where
  parseJSON = Aeson.genericParseJSON defaultOptions
    { sumEncoding = TaggedObject "type" "payload"
    }

handleDbError :: Db.SessionError -> a
handleDbError err = error [i|Unexpected database error: #{err}|]

workerJobName :: WorkerJob -> Text
workerJobName wj =
  case wj of
    SendEmail x -> tshow $ typeOf x
    ProcessAwsSnsWebhookMessage x -> tshow $ typeOf x
    Debug x -> tshow $ typeOf x


newtype DbNotification =
  DbNotification { microsecondsUntilRunAt :: Int }
  deriving (Generic, Show)

instance Aeson.FromJSON DbNotification
