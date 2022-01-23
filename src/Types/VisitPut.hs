module Types.VisitPut (VisitPut(..)) where

import           Data.Aeson            (FromJSON)
import           Data.Int              (Int64)
import           Data.Text             (Text, pack)
import           Data.Time             (UTCTime)
import           Data.Types.Isomorphic (Injective (to))
import           Data.UUID             (UUID)
import           GHC.Generics          (Generic)

import           Types.Visit           (VisitStatus (..), writeStatus)

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

instance Injective VisitPut (UUID, Text, Text, Text, Text, Bool) where
  to VisitPut{eventId, email, firstName, lastName, status, plusOne} = (eventId, email, firstName, lastName, writeStatus status, plusOne)

instance Injective VisitPut (UUID, Text, Text, Bool) where
  to VisitPut{eventId, email, status, plusOne} = (eventId, email, writeStatus status, plusOne)

instance Injective VisitPut (UUID, Text) where
  to VisitPut{eventId, email} = (eventId, email)
