module Types.Attendee (Attendee(..), AttendeeStatus(..), readStatus, writeStatus) where

import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Int                (Int64)
import           Data.String.Interpolate (iii)
import           Data.Text               (Text, pack, toTitle, unpack)
import           Data.Time               (UTCTime)
import           Data.Types.Isomorphic   (Injective (to), Iso)
import           Data.UUID               (UUID)
import           GHC.Generics            (Generic)

data AttendeeStatus = Coming
                 | MaybeComing
                 | NotComing
                 deriving (Eq, Read, Generic, Show)

instance ToJSON AttendeeStatus
instance FromJSON AttendeeStatus

data Attendee = Attendee
                { eventId   :: UUID
                , email     :: Text
                , firstName :: Text
                , lastName  :: Text
                , status    :: AttendeeStatus
                , plusOne   :: Bool
                , rsvpAt    :: UTCTime
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

instance Injective (UUID, Text, Text, Text, Text, Bool, UTCTime) Attendee where
  to (eventId, email, firstName, lastName, status, plusOne, rsvpAt) = Attendee eventId email firstName lastName (readStatus status) plusOne rsvpAt

instance Injective Attendee (UUID, Text, Text, Text, Text, Bool) where
  to Attendee{eventId, email, firstName, lastName, status, plusOne} = (eventId, email, firstName, lastName, writeStatus status, plusOne)
