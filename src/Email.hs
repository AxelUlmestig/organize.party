module Email (
  sendEmailInvitation,
  sendEventUpdateEmail,
  sendCommentNotifications,
  CommentNotificationRecipient(..),
  sendForgetMeConfirmation,
) where

import qualified Data.ByteString.Lazy     as LBS
import           Data.Foldable            (for_)
import           Data.String.Interpolate  (__i)
import           Data.Text
import           Data.Text.Lazy           (fromStrict)
import qualified Data.Text.Lazy           as LT
import           Data.Time.Clock          (UTCTime)
import           Data.Time.Format         (defaultTimeLocale, formatTime)
import           Data.Time.Format.ISO8601 (iso8601Show)
import           Data.UUID
import qualified Network.Mail.Mime        as Mail
import qualified Network.Mail.SMTP        as SMTP
import           Network.Socket           (PortNumber)

import           Types.AppEnv             (SmtpConfig (..))
import           Types.Attendee           (Attendee (..))
import           Types.CommentInput       (CommentInput (..))
import qualified Types.Event              as Event
import           Types.Event              (Event (..))

eventToICalendarString :: String -> Text -> Event -> LBS.ByteString
eventToICalendarString hostUrl email event@Event{Event.id = eid, startTime, endTime, title, description, location, createdAt, modifiedAt} =
  [__i|
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
    #{maybe "" (("DTEND:" <>) . formatICalendarTimestamp) endTime}
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

sendEmailInvitation ::  String -> SmtpConfig ->Event -> Attendee -> IO ()
sendEmailInvitation hostUrl SmtpConfig{server, port, login, password} event@Event{title} Attendee{email, name} = do
  let from       = SMTP.Address Nothing "noreply@organize.party"
  let to         = [SMTP.Address (Just name) email]
  let cc         = []
  let bcc        = []
  let subject    = title
  let body       = Mail.plainPart (formatDescription hostUrl event)
  let attachment = Mail.filePartBS "text/calendar" "invitation.ics" $ eventToICalendarString hostUrl email event

  let mail = SMTP.simpleMail from to cc bcc subject [body, attachment]

  SMTP.sendMailWithLogin' server port login password mail

sendEventUpdateEmail :: String -> SmtpConfig -> Event -> Attendee -> IO ()
sendEventUpdateEmail hostUrl SmtpConfig{server, port, login, password} event@Event{title} Attendee{email, name} = do
  let from       = SMTP.Address Nothing "noreply@organize.party"
  let to         = [SMTP.Address (Just name) email]
  let cc         = []
  let bcc        = []
  let subject    = title
  let body       = Mail.plainPart (formatDescription hostUrl event)
  let attachment = Mail.filePartBS "text/calendar" "invitation.ics" $ eventToICalendarString hostUrl email event

  let mail = SMTP.simpleMail from to cc bcc subject [body, attachment]

  SMTP.sendMailWithLogin' server port login password mail


formatICalendarTimestamp :: UTCTime -> String
formatICalendarTimestamp = formatTime defaultTimeLocale "%Y%m%dT%k%M%SZ"

data CommentNotificationRecipient =
  CommentNotificationRecipient
    { email         :: Text
    , recipientName :: Text
    , eventTitle    :: Text
    , forcePush     :: Bool
    }
    deriving (Eq, Show)

sendCommentNotifications :: String -> SmtpConfig -> CommentInput -> CommentNotificationRecipient -> IO ()
sendCommentNotifications
  hostUrl
  SmtpConfig{server, port, login, password}
  CommentInput{eventId, name, comment}
  CommentNotificationRecipient{email, recipientName, eventTitle, forcePush}
  = do
  let from       = SMTP.Address Nothing "noreply@organize.party"
  let cc         = []
  let bcc        = []
  let subject    = [__i|#{name} has left a comment on #{eventTitle}|]
  let to    = [SMTP.Address (Just recipientName) email]
  let mail  = SMTP.simpleMail from to cc bcc subject [emailBody]

  SMTP.sendMailWithLogin' server port login password mail
  where
    emailBody =
      Mail.htmlPart
        [__i|
          <b>#{name}</b> has left a comment on <a href="#{hostUrl}/e/#{eventId}">#{eventTitle}</a>
          <br>
          <br>
          <i>
            <pre>#{comment}</pre>
          </i>
          <br>
          <br>
          click the link below for more details
          <br>
          #{hostUrl}/e/#{eventId}
          #{unsubscribeInfo}
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


sendForgetMeConfirmation :: String -> SmtpConfig -> UUID -> Text -> IO ()
sendForgetMeConfirmation hostUrl SmtpConfig{server, port, login, password} forgetMeRequestId email = do
  let from       = SMTP.Address Nothing "noreply@organize.party"
  let to         = [SMTP.Address Nothing email]
  let cc         = []
  let bcc        = []
  let subject    = title

  let mail = SMTP.simpleMail from to cc bcc "Forget me request" [body]

  SMTP.sendMailWithLogin' server port login password mail
  where
    body =
      Mail.htmlPart
        [__i|
          A request to delete your data has been received. If you did not make
          this request, please ignore this email.

          If you did make this request, please click the link below to confirm. <b>Warning: this will delete all your data, it cannot be undone</b>
          <br>
          <a href="#{hostUrl}/forget-me/#{forgetMeRequestId}">#{hostUrl}/forget-me/#{forgetMeRequestId}</a>
          <br>
          <br>
          It will not delete events created by you, there's no connection between email addresses and events. It's impossible to tell which ones were created by you.
        |]

