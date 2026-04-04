module Op.Worker.JobLogistics (
  runLimitedParallelJobs,
  initSharedWorkerState,
  initiateShutdown,
  awaitShutdown,
  SharedWorkerState,
  ShutdownCircumstances (..),
) where

import qualified Control.Concurrent.STM as STM
import           RIO

maxNumberOfParallelJobs :: Int
maxNumberOfParallelJobs = 5

data ShutdownState
  = Running
  | Stopping
  | ForceStopping
  deriving (Eq, Show)

newtype SharedWorkerState = SharedWorkerState (STM.TVar (ShutdownState, Int))

data WorkerAction
  = DoJob
  | DoNothing

data ShutdownCircumstances
  = GracefulShutdown
  | ForcedShutdown

initSharedWorkerState :: MonadIO m => m SharedWorkerState
initSharedWorkerState = do
  liftIO $ fmap SharedWorkerState $ STM.atomically $ STM.newTVar (Running, 0)

-- | This will start gracefully shutting down the first time it's called.
-- If called again it will force shutdown immeadiately
initiateShutdown :: SharedWorkerState -> IO ()
initiateShutdown (SharedWorkerState tvar) = do
  atomically $ modifyTVar' tvar setStopping
  where
    setStopping (Stopping, activeJobs) =  (ForceStopping, activeJobs)
    setStopping (_, activeJobs)        =  (Stopping, activeJobs)

-- | This will block until the worker is stopping and no jobs are in progress.
-- Every time the shared worker state is updated it will check again
awaitShutdown :: MonadIO m => SharedWorkerState -> m ShutdownCircumstances
awaitShutdown (SharedWorkerState tvar) = do
  atomically $ do
    state <- readTVar tvar
    case state of
      (ForceStopping, _)                      -> pure ForcedShutdown
      (Stopping, activeJobs) | activeJobs < 1 -> pure GracefulShutdown
      _                                       -> STM.retry

-- | Ensures that there is a limit to how many jobs we run in parallel. It also
-- checks if the worker is shutting down and prevents new jobs from being
-- processed if that's the case.
--
-- Returns Nothing if no job is run
runLimitedParallelJobs :: (MonadIO m, MonadUnliftIO m) => SharedWorkerState -> m a -> m (Maybe a)
runLimitedParallelJobs (SharedWorkerState tvar) job = do
  workerAction <- liftIO $ STM.atomically $ STM.stateTVar tvar checkWorkerStatus
  case workerAction of
    DoNothing -> pure Nothing
    DoJob     -> do
      finally
        (Just <$> job)
        (liftIO $ STM.atomically $ STM.modifyTVar tvar restoreWorkerStatus)
  where
    checkWorkerStatus state =
      case state of
        (Running, activeJobs) | activeJobs < maxNumberOfParallelJobs ->
          (DoJob, (Running, activeJobs + 1))
        _ ->
          (DoNothing, state)
    restoreWorkerStatus (shutDownState, activeJobs) = (shutDownState, activeJobs - 1)

