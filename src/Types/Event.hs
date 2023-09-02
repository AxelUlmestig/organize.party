module Types.Event (Event(..), Attendee, Comment(..)) where

import           Data.Aeson            (ToJSON)
import           Data.Text             (Text)
import           Data.Time.Clock       (UTCTime)
import           Data.Types.Isomorphic (Injective (to), Iso)
import           Data.UUID             (UUID)
import           GHC.Generics          (Generic)
import           Types.Attendee        (AttendeeStatus (..), readStatus)

data Event
  = Event
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
    , comments       :: [Comment]
    }
    deriving (Generic, Eq)

data Attendee
  = Attendee
    { name    :: Text
    , status  :: AttendeeStatus
    , plusOne :: Bool
    }
    deriving (Generic, Eq)

data Comment
  = Comment
    { commenterName :: Text
    , comment       :: Text
    , timestamp     :: UTCTime
    , gravatarUrl   :: Text
    }
    deriving (Generic, Eq)

instance ToJSON Attendee
instance ToJSON Comment
instance ToJSON Event

instance Injective (UUID, Text, Text, UTCTime, Maybe UTCTime, Text, Maybe Text, UTCTime, UTCTime) Event where
  to (id, title, description, startTime, endTime, location, googleMapsLink, createdAt, modifiedAt) = Event id title description startTime endTime location googleMapsLink [] createdAt modifiedAt []

instance Injective (Text, Text, Bool) Attendee where
  to (name, status, plusOne) = Attendee{ status = readStatus status, ..}

instance Injective (Text, Text, UTCTime, Text) Comment where
  to (commenterName, comment, timestamp, gravatarUrl) = Comment {..}
