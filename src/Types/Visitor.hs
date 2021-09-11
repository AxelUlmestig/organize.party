{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.Visitor (Visitor(Visitor)) where

import           Data.Aeson                         (ToJSON)
import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import           GHC.Generics                       (Generic)

data Visitor = Visitor
             { id        :: Int
             , email     :: String
             , firstName :: String
             , lastName  :: String
             }
             deriving (Generic, Show)

instance ToJSON Visitor

instance FromRow Visitor where
  fromRow = Visitor <$> field <*> field <*> field <*> field
