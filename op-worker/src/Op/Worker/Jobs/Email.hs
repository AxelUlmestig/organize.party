{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}

module Op.Worker.Jobs.Email (
  SendEmailJob,
  SmtpConfig(..),
  HasSmtpConfig(..),
) where

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Reader     (asks)
import qualified Data.Aeson               as Aeson
import qualified Data.ByteString.Lazy     as LBS
import           Data.Foldable            (for_)
import           Data.Maybe               (fromMaybe)
import           Data.String.Interpolate  (__i, i)
import           Data.Text
import           Data.Text.Lazy           (fromStrict)
import qualified Data.Text.Lazy           as LT
import           Data.Time.Clock          (UTCTime, addUTCTime, nominalDay,
                                           secondsToNominalDiffTime)
import           Data.Time.Format         (defaultTimeLocale, formatTime)
import           Data.Time.Format.ISO8601 (iso8601Show)
import           Data.UUID
import qualified Data.Vector              as Vector
import           GHC.Generics             (Generic)
import qualified Hasql.Session            as Hasql
import           Hasql.TH                 (maybeStatement, vectorStatement)
import qualified Network.Mail.Mime        as Mail
import qualified Network.Mail.SMTP        as SMTP
import           Network.Socket           (PortNumber)
import           RIO

import qualified Op.Db                    as Db
import qualified Op.Worker.Job            as Job


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
    mEmail <- Db.queryDbOr retryDbErr session
    case mEmail of
      Nothing -> Job.giveUpJob [i|Couldn't find email with id: #{emailId}|]
      Just email -> do
        smtpConfig <- asks getSmtpConfig
        sendEmail smtpConfig email
    where
      session = do
        mEmail <- do
          Db.statement
            emailId
            [maybeStatement|
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
              [vectorStatement|
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
