{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.Event (Event(Event)) where

import           Data.Aeson                         (ToJSON)
import           Data.Time.Clock                    (UTCTime)
import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import           GHC.Generics                       (Generic)

data Event = Event
           { id             :: Int
           , title          :: String
           , description    :: String
           , startTime      :: UTCTime
           , endTime        :: UTCTime
           , location       :: String
           , googleMapsLink :: Maybe String
           }
           deriving (Show, Generic)

instance ToJSON Event

instance FromRow Event where
  fromRow = Event <$> field <*> field <*> field <*> field <*> field <*> field <*> field
