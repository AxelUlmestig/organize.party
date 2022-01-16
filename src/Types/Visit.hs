{-# LANGUAGE FlexibleInstances #-}

module Types.Visit (Visit(..), VisitStatus(..), fromTuple, toTuple) where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Int     (Int64)
import           Data.Text    (Text, pack, toTitle, unpack)
import           Data.Time    (UTCTime)
import           Data.UUID    (UUID)
import           GHC.Generics (Generic)

data VisitStatus = Coming
                 | MaybeComing
                 | NotComing
                 deriving (Eq, Read, Generic, Show)

instance ToJSON VisitStatus
instance FromJSON VisitStatus

data Visit = Visit
           { eventId   :: UUID
           , visitorId :: Int
           , status    :: VisitStatus
           , plusOne   :: Bool
           , rsvpAt    :: UTCTime
           }
           deriving (Generic, Show)

instance ToJSON Visit

fromTuple :: Integral a => (UUID, a, Text, Bool, UTCTime) -> Visit
fromTuple (eventId, visitorId, status, plusOne, rsvpAt) = Visit eventId (fromIntegral visitorId) (read . unpack . toTitle $ status) plusOne rsvpAt

toTuple :: Visit -> (UUID, Int64, Text, Bool)
toTuple Visit{eventId, visitorId, status, plusOne} = (eventId, fromIntegral visitorId, pack (show status), plusOne)
