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
           , endTime        :: UTCTime
           , location       :: Text
           , googleMapsLink :: Maybe Text
           , attendees      :: [Attendee]
           }
           deriving (Generic)

data Attendee = Attendee
                { firstName :: Text
                , lastName  :: Text
                , status    :: AttendeeStatus
                , plusOne   :: Bool
                }
                deriving (Generic)

instance ToJSON Attendee
instance ToJSON Event

instance Injective (UUID, Text, Text, UTCTime, UTCTime, Text, Maybe Text) Event where
  to (id, title, description, startTime, endTime, location, googleMapsLink) = Event id title description startTime endTime location googleMapsLink []

instance Injective (Text, Text, Text, Bool) Attendee where
  to (firstName, lastName, status, plusOne) = Attendee firstName lastName (readStatus status) plusOne
