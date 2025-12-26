{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE QuasiQuotes    #-}

module Main where

import           Data.ByteString.UTF8                as BSU
import qualified Data.Pool                           as Pool
import           Data.String.Interpolate             (i)
import qualified Data.Text                           as Text
import           Hasql.Connection                    (Connection, acquire)
import qualified Hasql.Connection.Setting            as ConnectionSetting
import qualified Hasql.Connection.Setting.Connection as ConnectionSettingConnection
import qualified Hasql.Notifications                 as Notifications
import qualified Hasql.Session                       as Hasql
import           Hasql.Statement                     (Statement)
import           Hasql.TH                            (maybeStatement,
                                                      resultlessStatement,
                                                      singletonStatement)
import qualified Op.Db                               as Db
import           System.Environment                  (lookupEnv)
import           System.Exit                         (die)
import           Text.Read                           (readMaybe)

import qualified Data.Aeson                          as Aeson
import           Data.Aeson.TH
import qualified Data.UUID                           as UUID
import           GHC.Generics
import           Prelude                             (putStrLn)
import           RIO

import qualified Op.Worker.Job                       as Job
import qualified Op.Worker.Jobs.Email                as Email

failedAttemptsLimit :: Int32
failedAttemptsLimit = 5

main :: IO ()
main = do
  connectionPool <- do
    dbSettings <- getDbConnectionSettings >>= either die pure
    Db.createPool dbSettings

  smtpConfig <- getSmtpSettings >>= either die pure

  -- We need a dedicated DB connection for listening for pg_notify. It can't go
  -- through PG Bouncer or connection pools or the connection will drop while
  -- we're listening and we won't get any notifications
  listenDbSettings <- getListenDbConnectionSettings >>= either die pure

  connection <- acquire listenDbSettings >>= either (die . show) pure
  Notifications.listen connection (Notifications.toPgIdentifier "new_worker_job")

  let handler _channel _payload = checkJobQueue connectionPool smtpConfig
  Notifications.waitForNotifications handler connection

checkJobQueue :: Pool.Pool Connection -> Email.SmtpConfig -> IO ()
checkJobQueue connectionPool smtpConfig = do
  checkAgain <- do
    runRIO connectionPool do
      Db.withDbConnection connectionPool \connection -> do
        -- We need to make sure that we use the same transaction to open and
        -- close the transaction, that's why the extra runRIO with one
        -- specific connection is needed
        runRIO connection do
          Db.beginTransactionOr undefined
          mJob <- Db.queryDbOr (liftIO . die . show) (Hasql.statement () checkJobQueueStatement)

          case mJob of
            Nothing -> do
              liftIO $ putStrLn [i|Didn't find any job, going back to sleep|]
              Db.commitTransactionOr undefined
              pure False; -- Don't check the queue for more jobs

            Just (jobId, failedAttempts, rawJobDefinition) -> do
              (mErrorMessage, updateJobStatement) <- do
                case Aeson.fromJSON rawJobDefinition of
                  Aeson.Error err -> do
                    pure (Just [i|jobId: #{jobId}, could not be parsed: #{err}|], moveToFailedJobStatement)

                  Aeson.Success (workerJob :: WorkerJob) -> do
                    eResult <- do
                      let workerEnv = WorkerEnv{jobId, connectionPool, smtpConfig}
                      Job.runJob workerEnv do
                        Job.processJob workerJob

                    pure $ case eResult of
                      Right () -> (Nothing, moveToCompletedJobStatement)
                      Left err ->
                        case err of
                          Job.NonRetryableError message ->
                            (Just message, moveToFailedJobStatement)
                          Job.RetryableError message | failedAttempts >= failedAttemptsLimit - 1 ->
                            (Just message, moveToFailedJobStatement)
                          Job.RetryableError message->
                            (Just message, returnJobToQueueStatement)

              void $ liftIO $ for mErrorMessage (putStrLn . Text.unpack)
              Db.queryDbOr undefined (Hasql.statement jobId updateJobStatement)
              Db.commitTransactionOr undefined
              pure True -- Do check the queue for more jobs

  when checkAgain do
    checkJobQueue connectionPool smtpConfig
  where
    checkJobQueueStatement =
      [maybeStatement|
        with jobs as (
          delete from queued_worker_jobs
          where id = (
            select id
            from queued_worker_jobs
            where run_at <= now()
            order by run_at asc, id
            limit 1
            for update skip locked
          )
          returning *
        )

        insert into in_progress_worker_jobs (
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
      [resultlessStatement|
        with
          jobs as (
            delete from in_progress_worker_jobs
            where id = $1::uuid
            returning *
          )

        insert into completed_worker_jobs (
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
      |]

    moveToFailedJobStatement =
      [resultlessStatement|
        with
          jobs as (
            delete from in_progress_worker_jobs
            where id = $1::uuid
            returning *
          )

        insert into failed_worker_jobs (
          id,
          run_at,
          failed_attempts,
          definition
        )
        select
          id,
          run_at,
          failed_attempts + 1,
          definition
        from jobs
      |]

    returnJobToQueueStatement =
      [resultlessStatement|
        with
          jobs as (
            delete from in_progress_worker_jobs
            where id = $1::uuid
            returning *
          )

        insert into queued_worker_jobs (
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



getListenDbConnectionSettings :: IO (Either String [ConnectionSetting.Setting])
getListenDbConnectionSettings = do
    mHost <- fmap BSU.fromString <$> lookupEnv "LISTEN_DB_HOST"
    mPort <- lookupEnv "LISTEN_DB_PORT"
    pure do
      host <- maybeToEither "Error: Missing env variable LISTEN_DB_HOST" mHost
      port :: Int <- maybeToEither "Error: Missing env variable LISTEN_DB_PORT" mPort >>= maybeToEither "Error: Couldn't parse port from LISTEN_DB_PORT" . readMaybe

      let connectionString = [i|host=#{host} dbname=events user=postgres password=postgres port=#{port}|]
      pure [ConnectionSetting.connection (ConnectionSettingConnection.string connectionString)]

getDbConnectionSettings :: IO (Either String [ConnectionSetting.Setting])
getDbConnectionSettings = do
    mHost <- fmap BSU.fromString <$> lookupEnv "DB_HOST"
    mPort <- lookupEnv "DB_PORT"
    pure do
      host <- maybeToEither "Error: Missing env variable DB_HOST" mHost
      port :: Int <- maybeToEither "Error: Missing env variable DB_PORT" mPort >>= maybeToEither "Error: Couldn't parse port from DB_PORT" . readMaybe

      let connectionString = [i|host=#{host} dbname=events user=postgres password=postgres port=#{port}|]
      pure [ConnectionSetting.connection (ConnectionSettingConnection.string connectionString)]


getSmtpSettings :: IO (Either String Email.SmtpConfig)
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

      pure Email.SmtpConfig{..}


maybeToEither :: err -> Maybe a -> Either err a
maybeToEither _ (Just a)  = Right a
maybeToEither err Nothing = Left err

data WorkerEnv = WorkerEnv
  { connectionPool :: Pool.Pool Connection
  , jobId          :: UUID.UUID
  , smtpConfig     :: Email.SmtpConfig
  }

instance Email.HasSmtpConfig WorkerEnv where
  getSmtpConfig = smtpConfig

instance Db.HasDbConnection WorkerEnv where
  withDbConnection WorkerEnv{connectionPool} =
    Db.withDbConnection connectionPool


data WorkerJob
  = SendEmail Email.SendEmailJob
  | Foo -- note to future me, if there's only one constructor then TH omits the WorkerJob layer when doing the JSON transformation
  deriving (Generic, Show)

instance Job.JobDefinition WorkerEnv WorkerJob where
  processJob wj =
    case wj of
      SendEmail sendEmail -> Job.processJob sendEmail
      Foo                 -> undefined

instance Aeson.ToJSON WorkerJob where
  toJSON = Aeson.genericToJSON defaultOptions
    { sumEncoding = TaggedObject "type" "payload"
    }

instance Aeson.FromJSON WorkerJob where
  parseJSON = Aeson.genericParseJSON defaultOptions
    { sumEncoding = TaggedObject "type" "payload"
    }

