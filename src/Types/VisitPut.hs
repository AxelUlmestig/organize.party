{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.VisitPut (VisitPut(..)) where

import           Data.Aeson   (FromJSON)
import           Data.Time    (UTCTime)
-- import           Database.PostgreSQL.Simple.FromField (FromField (..))
-- import           Database.PostgreSQL.Simple.FromRow   (FromRow (..), field)
import           GHC.Generics (Generic)

import           Types.Visit  (VisitStatus (..))

data VisitPut = VisitPut
              { eventId   :: Int
              , visitorId :: Int
              , status    :: VisitStatus
              , plusOne   :: Bool
              }
              deriving (Generic, Show)

instance FromJSON VisitPut
