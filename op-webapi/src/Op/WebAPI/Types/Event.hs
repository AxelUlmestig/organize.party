module Op.WebAPI.Types.Event (Event(..), Attendee, Comment(..)) where

import qualified Data.Aeson               as Aeson
import           Data.Char                (toLower)
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
    , photo          :: Maybe Photo
    }
    deriving (Generic, Eq, Show)

instance Aeson.ToJSON Event
instance Aeson.FromJSON Event

data Photo
  = Photo
  { photoId   :: UUID
  , photoUrl  :: Text
  , photoName :: Text
  }
  deriving (Eq, Show, Generic)

photoOptions :: Aeson.Options
photoOptions =
  let prefix = "photo" :: String
  in Aeson.defaultOptions { Aeson.fieldLabelModifier = lowerFirst . drop (length prefix) }
  where
    lowerFirst (c : cs) = toLower c : cs
    lowerFirst []       = []

instance Aeson.ToJSON Photo where
  toJSON     = Aeson.genericToJSON photoOptions
  toEncoding = Aeson.genericToEncoding photoOptions

instance Aeson.FromJSON Photo where
  parseJSON = Aeson.genericParseJSON photoOptions

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

