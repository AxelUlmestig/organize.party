{-# LANGUAGE FlexibleInstances #-}

module Types.CreateEventInput (CreateEventInput(..), toTuple) where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Text
import           Data.Time.Clock (UTCTime)
import           GHC.Generics    (Generic)

data CreateEventInput = CreateEventInput
                        { title          :: Text
                        , description    :: Text
                        , startTime      :: UTCTime
                        , endTime        :: UTCTime
                        , location       :: Text
                        , googleMapsLink :: Maybe Text
                        }
                        deriving (Generic)

instance ToJSON CreateEventInput
instance FromJSON CreateEventInput

toTuple :: CreateEventInput -> (Text, Text, UTCTime, UTCTime, Text, Maybe Text)
toTuple CreateEventInput{title, description, startTime, endTime, location, googleMapsLink} = (title, description, startTime, endTime, location, googleMapsLink)
