module Op.Worker.Job (
  Job,
  JobDefinition(..),
  JobErr(..),
  runJob,
  retryJob,
  giveUpJob,
) where

import qualified Control.Monad.Except   as Except
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Pool              as Pool
import           Data.Text              (Text)
import qualified Data.UUID              as UUID
import           Hasql.Connection       (Connection)
import qualified Op.Db                  as Db
import qualified RIO

type Job env a = Except.ExceptT JobErr (RIO.RIO env) a

data JobErr
  = RetryableError Text
  | NonRetryableError Text

retryJob :: Text -> Job env a
retryJob = Except.throwError . RetryableError

giveUpJob :: Text -> Job env a
giveUpJob = Except.throwError . NonRetryableError

runJob :: MonadIO m => env -> Job env a -> m (Either JobErr a)
runJob workerEnv = RIO.runRIO workerEnv . Except.runExceptT

class JobDefinition env a where
  processJob :: a -> Job env ()
