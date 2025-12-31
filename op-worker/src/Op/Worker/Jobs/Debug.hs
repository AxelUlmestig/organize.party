module Op.Worker.Jobs.Debug (
  DebugJob,
) where

import qualified Data.Aeson    as Aeson
import           RIO

import qualified Op.Worker.Job as Job

-- -- To queue a bunch of debug jobs
-- insert into queued_worker_jobs (definition)
-- values
--   ('{"type": "Debug", "payload": {"debugAction":"Succeed"}}'::jsonb),
--   ('{"type": "Debug", "payload": {"debugAction":"Succeed"}}'::jsonb),
--   ('{"type": "Debug", "payload": {"debugAction":"Succeed"}}'::jsonb);

data Action
  = Succeed
  | RetryableFail
  | NonRetryableFail
  deriving (Generic, Show)

newtype DebugJob = DebugJob { debugAction :: Action }
  deriving (Generic, Show)

instance Aeson.ToJSON Action
instance Aeson.ToJSON DebugJob
instance Aeson.FromJSON Action
instance Aeson.FromJSON DebugJob

instance HasLogFunc env => Job.JobDefinition env DebugJob where
  processJob (DebugJob action) = do
    logInfo "Doing debug job, will finish in two seconds..."
    liftIO $ threadDelay 2000000 -- two seconds
    case action of
      Succeed          -> pure ()
      RetryableFail    -> Job.retryJob "Failing debug job, trying again..."
      NonRetryableFail -> Job.giveUpJob "Failing debug job, not trying again"
