module Op.WebAPI.Types.CreateEventInput (CreateEventInput(..)) where

import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Text
import           Data.Time.Clock          (UTCTime)
import           Data.UUID                (UUID)
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
                        , photoId        :: Maybe UUID
                        }
                        deriving (Generic)

instance ToJSON CreateEventInput
instance FromJSON CreateEventInput

