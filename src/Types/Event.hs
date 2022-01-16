{-# LANGUAGE FlexibleInstances #-}

module Types.Event (Event(Event), fromTuple) where

import           Data.Aeson      (ToJSON)
import           Data.Text
import           Data.Time.Clock (UTCTime)
import           GHC.Generics    (Generic)

data Event = Event
           { id             :: Int
           , title          :: Text
           , description    :: Text
           , startTime      :: UTCTime
           , endTime        :: UTCTime
           , location       :: Text
           , googleMapsLink :: Maybe Text
           }
           deriving (Generic)

instance ToJSON Event

fromTuple :: Integral a => (a, Text, Text, UTCTime, UTCTime, Text, Maybe Text) -> Event
fromTuple (id, title, description, startTime, endTime, location, googleMapsLink) = Event (fromIntegral id) title description startTime endTime location googleMapsLink
