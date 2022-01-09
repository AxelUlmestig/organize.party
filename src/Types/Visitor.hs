{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.Visitor (Visitor(Visitor), fromTuple) where

import           Data.Aeson   (ToJSON)
-- import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data Visitor = Visitor
             { id        :: Int
             , email     :: Text
             , firstName :: Text
             , lastName  :: Text
             }
             deriving (Generic, Show)

instance ToJSON Visitor

fromTuple :: Integral a => (a, Text, Text, Text) -> Visitor
fromTuple (id, email, firstName, lastName) = Visitor (fromIntegral id) email firstName lastName

-- instance FromRow Visitor where
--   fromRow = Visitor <$> field <*> field <*> field <*> field
