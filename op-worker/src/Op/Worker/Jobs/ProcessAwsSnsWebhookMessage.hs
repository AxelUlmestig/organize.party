{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}

module Op.Worker.Jobs.ProcessAwsSnsWebhookMessage (
  ProcessAwsSnsWebhookMessageJob,
) where

import qualified Data.Aeson              as Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.List
import           Data.String.Interpolate (i)
import qualified Data.UUID               as UUID
import qualified Network.HTTP.Req        as Req
import           RIO
import           Text.URI                (mkURI)

import qualified Op.Db                   as Db
import qualified Op.Worker.Job           as Job

newtype ProcessAwsSnsWebhookMessageJob = ProcessAwsSnsWebhookMessageJob { awsSnsWebhookMessageId :: UUID.UUID }
  deriving (Generic, Show)

instance Aeson.ToJSON ProcessAwsSnsWebhookMessageJob
instance Aeson.FromJSON ProcessAwsSnsWebhookMessageJob

data NotificationSource
  = S3
  | SES
  deriving (Show)

getNotificationSource :: Aeson.Value -> Maybe NotificationSource
getNotificationSource message
  | isJust $ message ^? key "notificationType" = Just SES
  | message ^? key "Records" . nth 0 . key "eventSource" . _String == Just "aws:s3" = Just S3
  | otherwise = Nothing

instance (HasLogFunc env, Db.HasDbConnection env) => Job.JobDefinition env ProcessAwsSnsWebhookMessageJob where
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

    let mWebhookType = webhookContents ^? key "Type" . _String
    let mWebhookMessage = do
          messageText <- webhookContents ^? key "Message" . _String
          Aeson.decode . LBS.fromStrict . encodeUtf8 $ messageText

    case (mWebhookType, mWebhookMessage) of
      (Just "SubscriptionConfirmation", _) -> handleSubscriptionConfirmation webhookContents
      (Just "Notification", Just message) -> do
        case getNotificationSource message of
          Just SES -> do
            case message ^? key "notificationType" of
              Just "Delivery" -> handleSesDelivery message
              Just "Bounce" -> handleSesBounce message
              Just "Complaint" -> handleSesComplaint message
              _ -> Job.giveUpJob [i|Unexpected AWS SNS webhook (id: #{awsSnsWebhookMessageId}): #{Aeson.encode webhookContents}|]
          Just S3 -> do
            handleS3Notification message
          Nothing -> Job.giveUpJob [i|Unexpected AWS SNS webhook message (id: #{awsSnsWebhookMessageId}): #{Aeson.encode webhookContents}|]
      _ -> Job.giveUpJob [i|Unexpected AWS SNS webhook type (id: #{awsSnsWebhookMessageId}): #{Aeson.encode webhookContents}|]

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

  cleanUpJobAfterGivingUp (ProcessAwsSnsWebhookMessageJob awsSnsWebhookMessageId) = do
    Db.queryDbOr printDbError do
      Db.statement
        awsSnsWebhookMessageId
        [Db.resultlessStatement|
          select fsm.notify_state_machine(
            shard => 1,
            machine => state_machine_id,
            event => 'sns_webhook.processing_failed'
          )::text
          from aws.sns_webhook_messages
          where id = $1::uuid
        |]
      where
        printDbError err =
          logError
            [i|
            Unexpected db error when marking aws.sns_webhook_message processing as failed.
            aws.sns_webhook_messages.id = #{awsSnsWebhookMessageId}
            error message: #{err}
            |]

handleSubscriptionConfirmation :: Aeson.Value -> Job.Job env ()
handleSubscriptionConfirmation webhookContents = do
  rawSubscribeUrl <- do
    case webhookContents ^? key "SubscribeURL" . _String of
      Just subscribeUrl -> pure subscribeUrl
      _ -> Job.giveUpJob [i|Unexpected AWS SNS webhook: #{Aeson.encode webhookContents}|]

  (subscribeUrl, options) <- do
    case Req.useHttpsURI =<< mkURI rawSubscribeUrl of
      Just uri -> pure uri
      Nothing  -> Job.giveUpJob [i|Couldn't parse AWS SNS subsribe url: #{rawSubscribeUrl}|]

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

handleS3Notification :: (Db.HasDbConnection env) => Aeson.Value -> Job.Job env ()
handleS3Notification message = Job.giveUpJob [i|S3 notification job not implemented yet. Message: #{Aeson.encode message}|]

extractEmailId :: Aeson.Value -> Maybe UUID.UUID
extractEmailId webhookMessage = do
  headers <- webhookMessage ^? key "mail" . key "headers" . _Array
  let isEmailIdHeader h = h ^? key "name" . _String == Just "X-OP-EMAIL-ID"
  emailHeader <- Data.List.find isEmailIdHeader headers
  emailId <- emailHeader ^? key "value" . _String
  UUID.fromText emailId


handleSesDelivery :: (Db.HasDbConnection env) => Aeson.Value -> Job.Job env ()
handleSesDelivery webhookMessage = do
  emailId <- do
    case extractEmailId webhookMessage of
      Nothing -> Job.giveUpJob [i|No X-OP-EMAIL-ID found among email headers in delivery message from AWS SES: #{Aeson.encode webhookMessage}|]
      Just emailId -> pure emailId

  Db.queryDbOr retryDbErr do
    Db.statement
      emailId
      [Db.resultlessStatement|
          select fsm.notify_state_machine(
            shard => 1,
            machine => state_machine_id,
            event => 'email.received'
          )::text
          from email.emails
          where id = $1::uuid
      |]


handleSesBounce :: (Db.HasDbConnection env) => Aeson.Value -> Job.Job env ()
handleSesBounce webhookMessage = do
  emailId <- do
    case extractEmailId webhookMessage of
      Nothing -> Job.giveUpJob [i|No X-OP-EMAIL-ID found among email headers in bounce message from AWS SES: #{Aeson.encode webhookMessage}|]
      Just emailId -> pure emailId

  Db.queryDbOr retryDbErr do
    Db.statement
      (emailId, webhookMessage)
      [Db.resultlessStatement|
          select fsm.notify_state_machine(
            shard => 1,
            machine => state_machine_id,
            event => 'email.bounced',
            data => jsonb_build_object('block_context', $2::jsonb::text)
          )::text
          from email.emails
          where id = $1::uuid
      |]


handleSesComplaint :: (Db.HasDbConnection env) => Aeson.Value -> Job.Job env ()
handleSesComplaint webhookMessage = do
  emailId <- do
    case extractEmailId webhookMessage of
      Nothing -> Job.giveUpJob [i|No X-OP-EMAIL-ID found among email headers in complaint message from AWS SES: #{Aeson.encode webhookMessage}|]
      Just emailId -> pure emailId

  Db.queryDbOr retryDbErr do
    Db.statement
      (emailId, webhookMessage)
      [Db.resultlessStatement|
          select fsm.notify_state_machine(
            shard => 1,
            machine => state_machine_id,
            event => 'email.marked_as_spam',
            data => jsonb_build_object('block_context', $2::jsonb::text)
          )::text
          from email.emails
          where id = $1::uuid
      |]


retryDbErr :: Db.SessionError -> Job.Job env a
retryDbErr err = Job.retryJob [i|Error when accessing db for ProcessAwsSnsWebhookMessage job: #{err}|]
