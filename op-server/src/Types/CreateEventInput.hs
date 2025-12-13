module Types.CreateEventInput (CreateEventInput(..)) where

import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Text
import           Data.Time.Clock       (UTCTime)
import           Data.Types.Isomorphic (Injective (to), Iso)
import           GHC.Generics          (Generic)

data CreateEventInput = CreateEventInput
                        { title          :: Text
                        , description    :: Text
                        , startTime      :: UTCTime
                        , endTime        :: Maybe UTCTime
                        , location       :: Text
                        , googleMapsLink :: Maybe Text
                        , password       :: Text
                        }
                        deriving (Generic)

instance ToJSON CreateEventInput
instance FromJSON CreateEventInput

instance Injective CreateEventInput (Text, Text, UTCTime, Maybe UTCTime, Text, Maybe Text, Text) where
  to CreateEventInput{title, description, startTime, endTime, location, googleMapsLink, password} = (title, description, startTime, endTime, location, googleMapsLink, password)

