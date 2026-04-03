{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Types.Attendee (Attendee(..), AttendeeStatus(..), readStatus, writeStatus) where

import           Data.Aeson              (FromJSON, ToJSON)
import           Data.String.Interpolate (iii)
import           Data.Time               (UTCTime)
import           Data.UUID               (UUID)
import           RIO

data AttendeeStatus
  = Coming
  | MaybeComing
  | NotComing
  deriving (Eq, Read, Generic, Show)

instance ToJSON AttendeeStatus
instance FromJSON AttendeeStatus

data Attendee
  = Attendee
    { eventId       :: UUID
    , email         :: Text
    , name          :: Text
    , status        :: AttendeeStatus
    , plusOne       :: Bool
    , rsvpAt        :: UTCTime
    , unsubscribeId :: UUID
    }
    deriving (Generic, Show)

instance ToJSON Attendee

writeStatus :: AttendeeStatus -> Text
writeStatus Coming      = "coming"
writeStatus MaybeComing = "maybe_coming"
writeStatus NotComing   = "not_coming"

readStatus :: Text -> AttendeeStatus
readStatus "coming"       = Coming
readStatus "maybe_coming" = MaybeComing
readStatus "not_coming"   = NotComing
readStatus other          = error [iii|unknown AttendeeStatus: #{other}|]

