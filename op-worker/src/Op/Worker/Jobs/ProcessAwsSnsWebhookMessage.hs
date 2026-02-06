{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}

module Op.Worker.Jobs.ProcessAwsSnsWebhookMessage (
  ProcessAwsSnsWebhookMessageJob,
) where

import qualified Data.Aeson              as Aeson
import           Data.UUID (UUID)
import           RIO
-- import qualified Hasql.Session           as Hasql
import           Data.String.Interpolate (i)
import           Data.Aeson.Lens
import qualified Network.HTTP.Req as Req
import           Text.URI                (mkURI)

import qualified Op.Db                   as Db
import qualified Op.Worker.Job           as Job

newtype ProcessAwsSnsWebhookMessageJob = ProcessAwsSnsWebhookMessageJob { awsSnsWebhookMessageId :: UUID }
  deriving (Generic, Show)

instance Aeson.ToJSON ProcessAwsSnsWebhookMessageJob
instance Aeson.FromJSON ProcessAwsSnsWebhookMessageJob

instance (Db.HasDbConnection env) => Job.JobDefinition env ProcessAwsSnsWebhookMessageJob where
  processJob (ProcessAwsSnsWebhookMessageJob awsSnsWebhookMessageId) = do
    webhookContents <- do
      mContents <- Db.queryDbOr retryDbErr do
        Db.statement
          awsSnsWebhookMessageId
          [Db.maybeStatement|
            select contents::jsonb
            from aws.sns_webhook_messages
            where id = $1::uuid
          |]

      case mContents of
        Just contents -> pure contents
        Nothing -> Job.giveUpJob [i|Couldn't find aws.sns_webhook_messages where id = ${awsSnsWebhookMessageId}|]

    case webhookContents ^? key "SubscribeURL" . _String of
      Just subscribeUrl -> callSubscribeUrl subscribeUrl
      _ -> Job.giveUpJob [i|Unexpected AWS SNS webhook (id: #{awsSnsWebhookMessageId}): #{Aeson.encode webhookContents}|]

    Db.queryDbOr retryDbErr do
      Db.statement
        awsSnsWebhookMessageId
        [Db.resultlessStatement|
            select fsm.notify_state_machine(
              shard => 1,
              machine => state_machine_id,
              event => 'sns_webhook.processed'
            )::text
            from aws.sns_webhook_messages
            where id = $1::uuid
        |]

callSubscribeUrl :: Text -> Job.Job env ()
callSubscribeUrl rawSubscribeUrl = do
  (subscribeUrl, options) <- do
    case Req.useHttpsURI =<< mkURI rawSubscribeUrl of
      Just uri -> pure uri
      Nothing ->  Job.giveUpJob [i|Couldn't parse AWS SNS subsribe url: #{rawSubscribeUrl}|]

  response <- do
    liftIO do
      Req.runReq Req.defaultHttpConfig do
        Req.req
          Req.GET
          subscribeUrl
          Req.NoReqBody
          Req.ignoreResponse
          options

  case Req.responseStatusCode response `div` 100 of
    2 -> pure ()
    _ -> Job.retryJob [i|Unexpected HTTP response code when calling AWS SNS subscribe url: #{Req.responseStatusCode response}, trying again...|]

retryDbErr :: Db.SessionError -> Job.Job env a
retryDbErr err = Job.retryJob [i|Error when accessing db for ProcessAwsSnsWebhookMessage job: #{err}|]
