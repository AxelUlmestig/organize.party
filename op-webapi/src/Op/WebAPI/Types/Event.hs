module Op.WebAPI.Types.Event (Event(..), Attendee, Comment(..)) where

import qualified Data.Aeson               as Aeson
import           Data.Text                (Text)
import           Data.Time.Clock          (UTCTime)
import           Data.Types.Isomorphic    (Injective (to), Iso)
import           Data.UUID                (UUID)
import           GHC.Generics             (Generic)
import           Op.WebAPI.Types.Attendee (AttendeeStatus (..), readStatus)

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
    deriving (Generic, Eq, Show)

instance Aeson.ToJSON Event
instance Aeson.FromJSON Event

data Attendee
  = Attendee
    { name    :: Text
    , status  :: AttendeeStatus
    , plusOne :: Bool
    }
    deriving (Generic, Eq, Show)

instance Aeson.ToJSON Attendee
instance Aeson.FromJSON Attendee

data Comment
  = Comment
    { commenterName :: Text
    , comment       :: Text
    , timestamp     :: UTCTime
    , gravatarUrl   :: Maybe Text
    }
    deriving (Generic, Eq, Show)

instance Aeson.ToJSON Comment
instance Aeson.FromJSON Comment

instance Injective (UUID, Text, Text, UTCTime, Maybe UTCTime, Text, Maybe Text, UTCTime, UTCTime) Event where
  to (id, title, description, startTime, endTime, location, googleMapsLink, createdAt, modifiedAt) = Event id title description startTime endTime location googleMapsLink [] createdAt modifiedAt []

instance Injective (Text, Text, Bool) Attendee where
  to (name, status, plusOne) = Attendee{ status = readStatus status, ..}

instance Injective (Text, Text, UTCTime, Maybe Text) Comment where
  to (commenterName, comment, timestamp, gravatarUrl) = Comment {..}
