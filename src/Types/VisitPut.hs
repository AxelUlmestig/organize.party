{-# LANGUAGE FlexibleInstances #-}

module Types.VisitPut (VisitPut(..), toTuple) where

import           Data.Aeson   (FromJSON)
import           Data.Time    (UTCTime)
import           GHC.Generics (Generic)

import           Data.Int     (Int64)
import           Data.Text    (Text, pack)
import           Data.UUID    (UUID)
import           Types.Visit  (VisitStatus (..), writeStatus)

data VisitPut = VisitPut
              { eventId   :: UUID
              , visitorId :: Int
              , status    :: VisitStatus
              , plusOne   :: Bool
              }
              deriving (Generic, Show)

instance FromJSON VisitPut

toTuple :: VisitPut -> (UUID, Int64, Text, Bool)
toTuple VisitPut{eventId, visitorId, status, plusOne} = (eventId, fromIntegral visitorId, writeStatus status, plusOne)
