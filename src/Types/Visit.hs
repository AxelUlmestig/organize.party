{-# LANGUAGE FlexibleInstances #-}

module Types.Visit (Visit(..), VisitStatus(..), fromTuple) where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text, toTitle, unpack)
import           Data.Time    (UTCTime)
import           GHC.Generics (Generic)

data VisitStatus = Coming
                 | MaybeComing
                 | NotComing
                 deriving (Eq, Read, Generic, Show)

instance ToJSON VisitStatus
instance FromJSON VisitStatus

data Visit = Visit
           { eventId   :: Int
           , visitorId :: Int
           , status    :: VisitStatus
           , plusOne   :: Bool
           , rsvpAt    :: UTCTime
           }
           deriving (Generic, Show)

instance ToJSON Visit

fromTuple :: Integral a => (a, a, Text, Bool, UTCTime) -> Visit
fromTuple (eventId, visitorId, status, plusOne, rsvpAt) = Visit (fromIntegral eventId) (fromIntegral visitorId) (read . unpack . toTitle $ status) plusOne rsvpAt
