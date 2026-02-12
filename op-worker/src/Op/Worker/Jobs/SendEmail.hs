{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}

module Op.Worker.Jobs.SendEmail (
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

sendEmail :: MonadIO m => SmtpConfig -> UUID -> EmailContents -> m ()
sendEmail SmtpConfig{server, port, login, password} emailId EmailContents{recipientEmail, recipientName, subject, body, attachments} = do
  let mailFrom       = SMTP.Address Nothing "noreply@organize.party"
  let mailTo         = [SMTP.Address recipientName recipientEmail]
  let mailCc         = []
  let mailBcc        = []

  let mailParts =
        let attachments' = (\EmailAttachment{contentType, fileName, fileContents} -> Mail.filePartBS contentType fileName fileContents) <$> attachments
            body' = Mail.htmlPart $ fromStrict body
        in [body' : attachments']

  let mailHeaders = [("Subject", subject), ("X-OP-EMAIL-ID", tshow emailId)]

  let mail = Mail.Mail {..}

  -- AWS SES requires TLS, but when doing local test we can't use TLS
  case server of
    "localhost" ->  liftIO $ SMTP.sendMailWithLogin' server port login password mail
    _ ->            liftIO $ SMTP.sendMailWithLoginTLS' server port login password mail



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
    sendEmail smtpConfig emailId email

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

retryDbErr :: Db.SessionError -> Job.Job env a
retryDbErr err = Job.retryJob [i|Error when accessing db for email job: #{err}|]
