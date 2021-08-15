module Types.Visitor (Visitor(Visitor)) where

import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)

data Visitor = Visitor
             { id        :: Int
             , email     :: String
             , firstName :: String
             , lastName  :: String
             }
             deriving (Show)

instance FromRow Visitor where
  fromRow = Visitor <$> field <*> field <*> field <*> field
