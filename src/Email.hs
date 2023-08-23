module Email (sendEmailInvitation, sendEventUpdateEmail, sendCommentNotifications, CommentNotificationRecipient(..)) where

import qualified Data.ByteString.Lazy     as LBS
import           Data.Foldable            (for_)
import           Data.String.Interpolate  (__i)
import           Data.Text
import           Data.Text.Lazy           (fromStrict)
import qualified Data.Text.Lazy           as LT
import           Data.Time.Clock          (UTCTime)
import           Data.Time.Format         (defaultTimeLocale, formatTime)
import           Data.Time.Format.ISO8601 (iso8601Show)
import qualified Network.Mail.Mime        as Mail
import qualified Network.Mail.SMTP        as SMTP
import           Network.Socket           (PortNumber)

import           Types.AppEnv             (SmtpConfig (..))
import           Types.Attendee           (Attendee (..))
import           Types.CommentInput       (CommentInput (..))
import qualified Types.Event              as Event
import           Types.Event              (Event (..))

eventToICalendarString :: Text -> Event -> LBS.ByteString
eventToICalendarString email event@Event{Event.id = eid, startTime, endTime, title, description, location, createdAt, modifiedAt} =
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
    DESCRIPTION:#{LT.replace "\n" "\\n" (formatDescription event)}
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

formatDescription :: Event -> LT.Text
formatDescription Event{description, Event.id = eid} =
  [__i|
    #{description}

    https://organize.party/e/#{eid}
  |]

sendEmailInvitation :: SmtpConfig -> Event -> Attendee -> IO ()
sendEmailInvitation SmtpConfig{server, port, login, password} event@Event{title} Attendee{email, name} = do
  let from       = SMTP.Address Nothing "noreply@organize.party"
  let to         = [SMTP.Address (Just name) email]
  let cc         = []
  let bcc        = []
  let subject    = title
  let body       = Mail.plainPart (formatDescription event)
  let attachment = Mail.filePartBS "text/calendar" "invitation.ics" $ eventToICalendarString email event

  let mail = SMTP.simpleMail from to cc bcc subject [body, attachment]

  SMTP.sendMailWithLogin' server port login password mail

sendEventUpdateEmail :: SmtpConfig -> Event -> Attendee -> IO ()
sendEventUpdateEmail SmtpConfig{server, port, login, password} event@Event{title} Attendee{email, name} = do
  let from       = SMTP.Address Nothing "noreply@organize.party"
  let to         = [SMTP.Address (Just name) email]
  let cc         = []
  let bcc        = []
  let subject    = title
  let body       = Mail.plainPart (formatDescription event)
  let attachment = Mail.filePartBS "text/calendar" "invitation.ics" $ eventToICalendarString email event

  let mail = SMTP.simpleMail from to cc bcc subject [body, attachment]

  SMTP.sendMailWithLogin' server port login password mail


formatICalendarTimestamp :: UTCTime -> String
formatICalendarTimestamp = formatTime defaultTimeLocale "%Y%m%dT%k%M%SZ"

data CommentNotificationRecipient =
  CommentNotificationRecipient
    { email         :: Text
    , recipientName :: Text
    , eventTitle    :: Text
    }
    deriving (Eq, Show)

sendCommentNotifications :: SmtpConfig -> CommentInput -> CommentNotificationRecipient -> IO ()
sendCommentNotifications
  SmtpConfig{server, port, login, password}
  CommentInput{eventId, name, comment}
  CommentNotificationRecipient{email, recipientName, eventTitle}
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
          <b>#{name}</b> has left a comment on <a href="https://organize.party/e/#{eventId}">#{eventTitle}</a>
          <br>
          <br>
          <i>
            <pre>#{comment}</pre>
          </i>
          <br>
          <br>
          click the link below for more details
          <br>
          https://organize.party/e/#{eventId}
        |]

