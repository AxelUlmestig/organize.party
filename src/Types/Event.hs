{-# LANGUAGE FlexibleInstances #-}

module Types.Event (Event(Event), fromTuple) where

import           Data.Aeson      (ToJSON)
import           Data.Text
import           Data.Time.Clock (UTCTime)
import           Data.UUID       (UUID)
import           GHC.Generics    (Generic)

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

fromTuple :: (UUID, Text, Text, UTCTime, UTCTime, Text, Maybe Text) -> Event
fromTuple (id, title, description, startTime, endTime, location, googleMapsLink) = Event id title description startTime endTime location googleMapsLink
