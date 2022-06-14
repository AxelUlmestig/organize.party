module Email (eventToICalendarString) where

import           Data.String.Interpolate  (__i)
import           Data.Text
import           Data.Time.Format.ISO8601 (iso8601Show)
import qualified Network.Mail.SMTP        as SMTP
import           Types.Attendee           (Attendee (..))
import           Types.Event              (Event (..))
import qualified Types.Event              as Event

eventToICalendarString :: Event -> Text
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
  let body       = plainTextPart (formatDescription event)
  let html       = htmlPart "<h1>HTML</h1>"

  let mail = simpleMail from to cc bcc subject [body, html, attachment]

  SMTP.sendMail SMTP.host mail
