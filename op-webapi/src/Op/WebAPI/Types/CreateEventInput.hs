module Op.WebAPI.Types.CreateEventInput (CreateEventInput(..)) where

import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Text
import           Data.Time.Clock          (UTCTime)
import           Data.Types.Isomorphic    (Injective (to))
import           RIO

import           Op.WebAPI.Types.Password (Password (..))

data CreateEventInput = CreateEventInput
                        { title          :: Text
                        , description    :: Text
                        , startTime      :: UTCTime
                        , endTime        :: Maybe UTCTime
                        , location       :: Text
                        , googleMapsLink :: Maybe Text
                        , password       :: Password
                        }
                        deriving (Generic)

instance ToJSON CreateEventInput
instance FromJSON CreateEventInput

instance Injective CreateEventInput (Text, Text, UTCTime, Maybe UTCTime, Text, Maybe Text, Text) where
  -- to CreateEventInput{title, description, startTime, endTime, location, googleMapsLink, password} = (title, description, startTime, endTime, location, googleMapsLink, password)
  to CreateEventInput{title, description, startTime, endTime, location, googleMapsLink, password = Password password} = (title, description, startTime, endTime, location, googleMapsLink, password)

