module Op.Worker.Job (
  Job,
  JobDefinition(..),
  JobErr(..),
  runJob,
  retryJob,
  giveUpJob,
  finallyJ,
) where

import qualified Control.Monad.Except as Except
import           RIO

type Job env a = Except.ExceptT JobErr (RIO env) a

data JobErr
  = RetryableError Utf8Builder
  | NonRetryableError Utf8Builder

retryJob :: Utf8Builder -> Job env a
retryJob = Except.throwError . RetryableError

giveUpJob :: Utf8Builder -> Job env a
giveUpJob = Except.throwError . NonRetryableError

runJob :: MonadIO m => env -> Job env a -> m (Either JobErr a)
runJob workerEnv = RIO.runRIO workerEnv . Except.runExceptT

class JobDefinition env a where
  processJob :: a -> Job env ()

finallyJ ::
  Job env a ->
  RIO env b ->
  Job env a
finallyJ action after = do
  eResult <- lift do
    finally
      (Except.runExceptT action)
      after
  Except.liftEither eResult

