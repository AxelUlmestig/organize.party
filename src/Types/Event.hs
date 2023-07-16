module Types.Event (Event(..), Attendee) where

import           Data.Aeson            (ToJSON)
import           Data.Text             (Text)
import           Data.Time.Clock       (UTCTime)
import           Data.Types.Isomorphic (Injective (to), Iso)
import           Data.UUID             (UUID)
import           GHC.Generics          (Generic)
import           Types.Attendee        (AttendeeStatus (..), readStatus)

data Event = Event
           { id             :: UUID
           , title          :: Text
           , description    :: Text
           , startTime      :: UTCTime
           , endTime        :: Maybe UTCTime
           , location       :: Text
           , googleMapsLink :: Maybe Text
           , attendees      :: [Attendee]
           , createdAt      :: UTCTime
           , modifiedAt     :: UTCTime
           }
           deriving (Generic, Eq)

data Attendee = Attendee
                { name    :: Text
                , status  :: AttendeeStatus
                , comment :: Maybe Text
                , plusOne :: Bool
                }
                deriving (Generic, Eq)

instance ToJSON Attendee
instance ToJSON Event

instance Injective (UUID, Text, Text, UTCTime, Maybe UTCTime, Text, Maybe Text, UTCTime, UTCTime) Event where
  to (id, title, description, startTime, endTime, location, googleMapsLink, createdAt, modifiedAt) = Event id title description startTime endTime location googleMapsLink [] createdAt modifiedAt

instance Injective (Text, Text, Maybe Text, Bool) Attendee where
  to (name, status, comment, plusOne) = Attendee{ status = readStatus status, ..}
