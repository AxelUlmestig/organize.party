{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}

module Op.Worker.Jobs.Email (
  SendEmailJob,
  SmtpConfig(..),
  HasSmtpConfig(..),
) where

import qualified Data.Aeson              as Aeson
import qualified Data.ByteString.Lazy    as LBS
import           Data.String.Interpolate (i)
import           Data.Text
import           Data.Text.Lazy          (fromStrict)
import           Data.UUID
import qualified Data.Vector             as Vector
import qualified Hasql.Session           as Hasql
import qualified Network.Mail.Mime       as Mail
import qualified Network.Mail.SMTP       as SMTP
import           Network.Socket          (PortNumber)
import           RIO

import qualified Op.Db                   as Db
import qualified Op.Worker.Job           as Job


data SmtpConfig = SmtpConfig
  { server   :: String
  , port     :: PortNumber
  , login    :: String
  , password :: String
  }

class HasSmtpConfig env where
  getSmtpConfig :: env -> SmtpConfig

data EmailContents = EmailContents
  { recipientEmail :: Text
  , recipientName  :: Maybe Text
  , subject        :: Text
  , body           :: Text
  , attachments    :: [EmailAttachment]
  }

data EmailAttachment = EmailAttachment
  { contentType  :: Text
  , fileName     :: Text
  , fileContents :: LBS.ByteString
  }

sendEmail :: MonadIO m => SmtpConfig -> EmailContents -> m ()
sendEmail SmtpConfig{server, port, login, password} EmailContents{recipientEmail, recipientName, subject, body, attachments} = do
  let from       = SMTP.Address Nothing "noreply@organize.party"
  let to         = [SMTP.Address recipientName recipientEmail]
  let cc         = []
  let bcc        = []

  let attachments' = (\EmailAttachment{contentType, fileName, fileContents} -> Mail.filePartBS contentType fileName fileContents) <$> attachments
  let body' = Mail.htmlPart $ fromStrict body

  let mail = SMTP.simpleMail from to cc bcc subject (body' : attachments')

  liftIO $ SMTP.sendMailWithLogin' server port login password mail


newtype SendEmailJob = SendEmailJob { emailId :: UUID }
  deriving (Generic, Show)

instance Aeson.ToJSON SendEmailJob
instance Aeson.FromJSON SendEmailJob

instance (HasSmtpConfig env, Db.HasDbConnection env) => Job.JobDefinition env SendEmailJob where
  processJob (SendEmailJob emailId) = do
    email <- do
      mEmail <- Db.queryDbOr retryDbErr getEmailSession
      case mEmail of
        Nothing    -> Job.giveUpJob [i|Couldn't find email with id: #{emailId}|]
        Just email -> pure email

    smtpConfig <- asks getSmtpConfig
    sendEmail smtpConfig email

    Db.queryDbOr retryDbErr do
      Db.statement
        emailId
        [Db.resultlessStatement|
          select fsm.notify_state_machine(
            shard => 1,
            machine => state_machine_id,
            event => 'email.sent'
          )::text
          from email.emails
          where id = $1::uuid
        |]
    where
      getEmailSession = do
        mEmail <- do
          Db.statement
            emailId
            [Db.maybeStatement|
              select
                recipient_email::text,
                recipient_name::text?,
                subject::text,
                body::text
              from email.emails
              where id = $1::uuid
            |]

        for mEmail \(recipientEmail, recipientName, subject, body) -> do
          rawAttachments <- Vector.toList <$> do
            Db.statement
              emailId
              [Db.vectorStatement|
                select
                  content_type::text,
                  file_name::text,
                  file_contents::bytea
                from email.email_attachments
                where email_id = $1::uuid
              |]

          let attachments = (\(ct, fn, fc) -> EmailAttachment ct fn (LBS.fromStrict fc)) <$> rawAttachments

          pure EmailContents {..}

retryDbErr :: Hasql.SessionError -> Job.Job env a
retryDbErr err = Job.retryJob [i|Error when accessing db for email job: #{err}|]
