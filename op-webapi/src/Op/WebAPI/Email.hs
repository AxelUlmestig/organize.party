{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Email (
  sendEmailInvitation,
  sendEventUpdateEmail,
  sendCommentNotifications,
  CommentNotificationRecipient(..),
  EmailData(..),
  sendForgetMeConfirmation,
) where

import qualified Data.ByteString              as BS
import           Data.Maybe                   (fromMaybe)
import           Data.String.Interpolate      (__i)
import           Data.Text
import qualified Data.Text.Lazy               as LT
import           Data.Time.Clock              (UTCTime, addUTCTime,
                                               secondsToNominalDiffTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Data.Time.Format.ISO8601     (iso8601Show)
import           Data.UUID
import           Hasql.TH                     (resultlessStatement)
import qualified Network.Mail.Mime            as Mail
import           Network.Socket               (PortNumber)
import qualified RIO

import qualified Op.Db                        as Db
import           Op.WebAPI.Types.AppEnv       (SmtpConfig (..))
import           Op.WebAPI.Types.Attendee     (Attendee (..))
import           Op.WebAPI.Types.CommentInput (CommentInput (..))
import           Op.WebAPI.Types.Event        (Event (..))
import qualified Op.WebAPI.Types.Event        as Event

data EmailData
  = EmailData
    { email         :: Text
    , recipientName :: Text
    , emailHostUrl  :: String
    , unsubscribeId :: UUID
    }

eventToICalendarString :: String -> Text -> Event -> BS.ByteString
eventToICalendarString hostUrl email event@Event{Event.id = eid, startTime, endTime, title, description, location, createdAt, modifiedAt} =
  let
    oneHour = secondsToNominalDiffTime $ 60 ^ 2
  in [__i|
    BEGIN:VCALENDAR

    CALSCALE:GREGORIAN
    VERSION:2.0
    PRODID:-//organize.party/event//calendar//EN
    METHOD:REQUEST

    BEGIN:VEVENT

    UID:#{eid}
    X-MICROSOFT-CDO-OWNERAPPTID:#{eid}

    DTSTAMP:#{formatICalendarTimestamp modifiedAt}
    ORGANIZER;CN=organize.party:MAILTO:noreply@organize.party
    DTSTART:#{formatICalendarTimestamp startTime}
    DTEND:#{formatICalendarTimestamp (fromMaybe (addUTCTime oneHour startTime) endTime)}
    SUMMARY:#{title}
    DESCRIPTION:#{LT.replace "\n" "\\n" (formatDescription hostUrl event)}
    CREATED:#{formatICalendarTimestamp createdAt}
    LAST-MODIFIED:#{formatICalendarTimestamp modifiedAt}
    LOCATION:#{location}
    SEQUENCE:0

    STATUS:CONFIRMED
    TRANSP:TRANSPARENT

    ATTENDEE;CUTYPE=INDIVIDUAL;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;RSVP=TRUE
     ;CN=#{email};X-NUM-GUESTS=0:mailto:#{email}

    END:VEVENT

    END:VCALENDAR
  |]

formatDescription :: String -> Event -> LT.Text
formatDescription hostUrl Event{description, Event.id = eid} =
  [__i|
    #{description}

    #{hostUrl}/e/#{eid}
  |]

sendEmailInvitation
  :: (
    Db.HasDbConnection env,
    RIO.MonadIO m,
    RIO.MonadReader env m
  ) =>
  EmailData ->
  Event ->
  m ()
sendEmailInvitation EmailData{email, recipientName, emailHostUrl, unsubscribeId} event@Event{title, description, id = eid} = do
  let icalendarString = eventToICalendarString emailHostUrl email event

  Db.queryDbOr (error . show) (Db.statement (email, recipientName, title, body, icalendarString) statement)

  where
    statement =
      [resultlessStatement|
        with
          inserted as (
            insert into email.emails (
              recipient_email,
              recipient_name,
              subject,
              body
            )
            values (
              $1::text,
              $2::text,
              $3::text,
              $4::text
            )
            returning id
          )

        insert into email.email_attachments (
          email_id,
          content_type,
          file_name,
          file_contents
        )
        select
          inserted.id,
          'text/calendar',
          'invitation.ics',
          $5::bytea
        from inserted
      |]

    body =
      [__i|
          #{description}
          <br>
          <br>
          <a href="#{emailHostUrl}/e/#{eid}">#{emailHostUrl}/e/#{eid}</a>
          <br>
          <br>
          <div style="font-size: x-small">
            If you never want to receive an email from this event again, <a href="#{emailHostUrl}/unsubscribe/#{unsubscribeId}">click here to unsubscribe</a>. Warning, this can not be undone
          </div>
        |]



sendEventUpdateEmail :: (Db.HasDbConnection env, RIO.MonadIO m, RIO.MonadReader env m) => EmailData -> Event -> m ()
sendEventUpdateEmail EmailData{email, recipientName, emailHostUrl, unsubscribeId} event@Event{title, id = eid, description} = do
  let icalendarString = eventToICalendarString emailHostUrl email event
  Db.queryDbOr undefined (Db.statement (email, recipientName, title, body, icalendarString) statement)
  where
    statement =
      [resultlessStatement|
        with
          inserted as (
            insert into email.emails (
              recipient_email,
              recipient_name,
              subject,
              body
            )
            values (
              $1::text,
              $2::text,
              $3::text,
              $4::text
            )
            returning id
          )

        insert into email.email_attachments (
          email_id,
          content_type,
          file_name,
          file_contents
        )
        select
          inserted.id,
          'text/calendar',
          'invitation.ics',
          $5::bytea
        from inserted
      |]

    body =
      [__i|
          #{description}
          <br>
          <br>
          <a href="#{emailHostUrl}/e/#{eid}">#{emailHostUrl}/e/#{eid}</a>
          <br>
          <br>
          <div style="font-size: x-small">
            If you never want to receive an email from this event again, <a href="#{emailHostUrl}/unsubscribe/#{unsubscribeId}">click here to unsubscribe</a>. Warning, this can not be undone
          </div>
        |]

formatICalendarTimestamp :: UTCTime -> String
formatICalendarTimestamp = formatTime defaultTimeLocale "%Y%m%dT%k%M%SZ"

data CommentNotificationRecipient =
  CommentNotificationRecipient
    { eventTitle :: Text
    , forcePush  :: Bool
    }
    deriving (Eq, Show)

sendCommentNotifications :: (Db.HasDbConnection env, RIO.MonadIO m, RIO.MonadReader env m) => EmailData -> CommentInput -> CommentNotificationRecipient -> m ()
sendCommentNotifications
  EmailData{emailHostUrl, email, recipientName, unsubscribeId}
  CommentInput{eventId, name, comment}
  CommentNotificationRecipient{eventTitle, forcePush}
  = do
    let subject    = [__i|#{name} has left a comment on #{eventTitle}|]
    Db.queryDbOr undefined (Db.statement (email, recipientName, subject, emailBody) statement)
  where
    statement =
      [resultlessStatement|
        insert into email.emails (
          recipient_email,
          recipient_name,
          subject,
          body
        )
        values (
          $1::text,
          $2::text,
          $3::text,
          $4::text
        )
      |]
    emailBody =
        [__i|
          <b>#{name}</b> has left a comment on <a href="#{emailHostUrl}/e/#{eventId}">#{eventTitle}</a>
          <br>
          <br>
          <i>
            <pre>#{comment}</pre>
          </i>
          <br>
          <br>
          #{unsubscribeInfo}
          <br>
          <br>
          <div style="font-size: x-small">
            If you never want to receive an email from this event again, <a href="#{emailHostUrl}/unsubscribe/#{unsubscribeId}">click here to unsubscribe</a>. Warning, this can not be undone
          </div>
        |]
      where
        unsubscribeInfo =
          if forcePush then
            [__i|
              <br>
              <br>
              <b>#{name}</b> chose to notify you of their comment by clicking the <i>send email notification to everyone?</i> checkbox
            |] :: Text
          else
            [__i|
              <br>
              <br>
              You can unsubscribe from these messages by unclicking the <i>get notified on comments?</i> checkbox and resubmitting your RSVP
            |]


sendForgetMeConfirmation :: (Db.HasDbConnection env, RIO.MonadIO m, RIO.MonadReader env m) => String -> UUID -> Text -> m ()
sendForgetMeConfirmation hostUrl forgetMeRequestId email = do
  let subject = "Forget me request"
  Db.queryDbOr undefined (Db.statement (email, subject, body) statement)
  where
    body =
      [__i|
        A request to delete your data has been received. If you did not make
        this request, please ignore this email.
        <br>
        <br>
        If you did make this request, please click the link below to confirm. <b>Warning: this will delete all your data, it cannot be undone</b>
        <br>
        <a href="#{hostUrl}/forget-me/#{forgetMeRequestId}">#{hostUrl}/forget-me/#{forgetMeRequestId}</a>
        <br>
        <br>
        It will not delete events created by you, there's no connection between email addresses and events. It's impossible to tell which ones were created by you.
      |]
    statement =
      [resultlessStatement|
        insert into email.emails (
          recipient_email,
          subject,
          body
        )
        values (
          $1::text,
          $2::text,
          $3::text
        )
      |]
