module Email (eventToICalendarString, sendEmailInvitation, SmtpConfig(..)) where

import qualified Data.ByteString.Lazy     as LBS
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

import           Types.Attendee           (Attendee (..))
import           Types.Event              (Event (..))
import qualified Types.Event              as Event

data SmtpConfig = SmtpConfig
  { server   :: String
  , port     :: PortNumber
  , login    :: String
  , password :: String
  }

eventToICalendarString :: Event -> LBS.ByteString
eventToICalendarString event@Event{Event.id = eid, startTime, endTime, title, description, location} =
  [__i|
    BEGIN:VCALENDAR
    VERSION:2.0
    PRODID:-//hacksw/handcal//NONSGML v1.0//EN
    BEGIN:VEVENT
    UID:#{eid}
    DTSTAMP:#{formatICalendarTimestamp startTime}
    ORGANIZER;CN=John Doe:MAILTO:john.doe@example.com
    DTSTART:#{formatICalendarTimestamp startTime}
    #{maybe "" (("DTEND:" <>) . formatICalendarTimestamp) endTime}
    SUMMARY:#{title}
    DESCRIPTION:#{LT.replace "\n" "\\n" (formatDescription event)}
    LOCATION:#{location}
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
  let attachment = Mail.filePartBS "text/calendar" "invitation.ics" $ eventToICalendarString event

  let mail = SMTP.simpleMail from to cc bcc subject [body, attachment]

  SMTP.sendMailWithLogin' server port login password mail

formatICalendarTimestamp :: UTCTime -> String
formatICalendarTimestamp = formatTime defaultTimeLocale "%Y%m%dT%k%M%SZ"
