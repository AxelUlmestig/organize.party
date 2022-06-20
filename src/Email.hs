module Email (eventToICalendarString, sendEmailInvitation) where

import qualified Data.ByteString.Lazy     as LBS
import           Data.String.Interpolate  (__i)
import           Data.Text
import           Data.Text.Lazy           (fromStrict)
import           Data.Time.Format.ISO8601 (iso8601Show)
import qualified Network.Mail.Mime        as Mail
import qualified Network.Mail.SMTP        as SMTP

import           Types.Attendee           (Attendee (..))
import           Types.Event              (Event (..))
import qualified Types.Event              as Event

eventToICalendarString :: Event -> LBS.ByteString
eventToICalendarString event@Event{Event.id = eid, startTime, endTime, title, description, location} =
  [__i|
    BEGIN:VCALENDAR
    VERSION:2.0
    PRODID:-//hacksw/handcal//NONSGML v1.0//EN
    BEGIN:VEVENT
    UID:${eid}
    DTSTAMP:#{startTime}
    ORGANIZER;CN=John Doe:MAILTO:john.doe@example.com
    DTSTART:#{startTime}
    #{maybe "" (("DTEND:" <>) . iso8601Show) endTime}
    SUMMARY:#{title}
    DESCRIPTION:#{formatDescription event}
    LOCATION:#{location}
    END:VEVENT
    END:VCALENDAR
  |]

formatDescription :: Event -> Text
formatDescription Event{description, Event.id = eid} =
  [__i|
    #{description}

    https://organize.party/e/#{eid}
  |]

sendEmailInvitation :: Event -> Attendee -> IO ()
sendEmailInvitation event@Event{title} Attendee{email, name} = do
  let from       = SMTP.Address Nothing "noreply@organize.party"
  let to         = [SMTP.Address (Just name) email]
  let cc         = []
  let bcc        = []
  let subject    = title
  let body       = Mail.plainPart (fromStrict (formatDescription event))
  let html       = Mail.htmlPart "<h1>You have been invited without port</h1>"
  let attachment = Mail.filePartBS "text/calendar" "invitation.ics" $ eventToICalendarString event

  let mail = SMTP.simpleMail from to cc bcc subject [body, html, attachment]

  SMTP.sendMailWithLogin "mail.mydomain.se" "axel@mydomain.se" "secretpassword" mail
