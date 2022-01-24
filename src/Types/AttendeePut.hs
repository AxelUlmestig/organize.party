module Types.AttendeePut (AttendeePut(..)) where

import           Data.Aeson            (FromJSON)
import           Data.Int              (Int64)
import           Data.Text             (Text, pack)
import           Data.Time             (UTCTime)
import           Data.Types.Isomorphic (Injective (to))
import           Data.UUID             (UUID)
import           GHC.Generics          (Generic)

import           Types.Attendee        (AttendeeStatus (..), writeStatus)

data AttendeePut = AttendeePut
                 { eventId   :: UUID
                 , email     :: Text
                 , firstName :: Text
                 , lastName  :: Text
                 , status    :: AttendeeStatus
                 , plusOne   :: Bool
                 }
                 deriving (Generic, Show)

instance FromJSON AttendeePut

instance Injective AttendeePut (UUID, Text, Text, Text, Text, Bool) where
  to AttendeePut{eventId, email, firstName, lastName, status, plusOne} = (eventId, email, firstName, lastName, writeStatus status, plusOne)

instance Injective AttendeePut (UUID, Text, Text, Bool) where
  to AttendeePut{eventId, email, status, plusOne} = (eventId, email, writeStatus status, plusOne)

instance Injective AttendeePut (UUID, Text) where
  to AttendeePut{eventId, email} = (eventId, email)
