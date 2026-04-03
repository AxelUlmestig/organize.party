module Op.WebAPI.Types.Event (Event(..), Attendee, Comment(..)) where

import qualified Data.Aeson               as Aeson
import           Data.Time.Clock          (UTCTime)
import           Data.UUID                (UUID)
import           Op.WebAPI.Types.Attendee (AttendeeStatus (..))
import           RIO

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

