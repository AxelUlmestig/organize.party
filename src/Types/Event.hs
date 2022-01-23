module Types.Event (Event(Event)) where

import           Data.Aeson            (ToJSON)
import           Data.Text
import           Data.Time.Clock       (UTCTime)
import           Data.Types.Isomorphic (Injective (to), Iso)
import           Data.UUID             (UUID)
import           GHC.Generics          (Generic)

data Event = Event
           { id             :: UUID
           , title          :: Text
           , description    :: Text
           , startTime      :: UTCTime
           , endTime        :: UTCTime
           , location       :: Text
           , googleMapsLink :: Maybe Text
           }
           deriving (Generic)

instance ToJSON Event

instance Injective (UUID, Text, Text, UTCTime, UTCTime, Text, Maybe Text) Event where
  to (id, title, description, startTime, endTime, location, googleMapsLink) = Event id title description startTime endTime location googleMapsLink

instance Injective Event (UUID, Text, Text, UTCTime, UTCTime, Text, Maybe Text) where
  to Event{Types.Event.id, title, description, startTime, endTime, location, googleMapsLink} = (id, title, description, startTime, endTime, location, googleMapsLink)

instance Iso Event (UUID, Text, Text, UTCTime, UTCTime, Text, Maybe Text)
