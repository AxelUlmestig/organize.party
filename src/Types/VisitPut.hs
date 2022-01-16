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
              , email     :: Text
              , firstName :: Text
              , lastName  :: Text
              , status    :: VisitStatus
              , plusOne   :: Bool
              }
              deriving (Generic, Show)

instance FromJSON VisitPut

toTuple :: VisitPut -> (UUID, Text, Text, Text, Text, Bool)
toTuple VisitPut{eventId, email, firstName, lastName, status, plusOne} = (eventId, email, firstName, lastName, writeStatus status, plusOne)
