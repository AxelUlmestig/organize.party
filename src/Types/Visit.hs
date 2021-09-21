{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Visit (Visit(..), VisitStatus(..)) where

import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Time                          (UTCTime)
import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import           Database.PostgreSQL.Simple.ToField
import           GHC.Generics                       (Generic)
import           Text.Casing                        (fromSnake, toPascal)

data VisitStatus = Coming
                 | MaybeComing
                 | NotComing
                 deriving (Eq, Read, Generic, Show)

instance ToJSON VisitStatus
instance FromJSON VisitStatus
instance ToField VisitStatus where
  toField Coming      = Plain "'coming'::visit_status"
  toField MaybeComing = Plain "'maybe_coming'::visit_status"
  toField NotComing   = Plain "'not_coming'::visit_status"


data Visit = Visit
           { eventId   :: Int
           , visitorId :: Int
           , status    :: VisitStatus
           , plusOne   :: Bool
           , rsvpAt    :: UTCTime
           }
           deriving (Generic, Show)

instance ToJSON Visit

instance FromRow Visit where
  fromRow = Visit <$> field <*> field <*> (read . toPascal . fromSnake <$> field) <*> field <*> field
