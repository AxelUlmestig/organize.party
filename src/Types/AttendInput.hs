module Types.AttendInput (AttendInput(..)) where

import           Data.Aeson            (FromJSON)
import           Data.Int              (Int64)
import           Data.Text             (Text, pack)
import           Data.Time             (UTCTime)
import           Data.Types.Isomorphic (Injective (to))
import           Data.UUID             (UUID)
import           GHC.Generics          (Generic)

import           Types.Attendee        (AttendeeStatus (..), writeStatus)

data AttendInput = AttendInput
                   { eventId   :: UUID
                   , email     :: Text
                   , firstName :: Text
                   , lastName  :: Text
                   , status    :: AttendeeStatus
                   , plusOne   :: Bool
                   }
                   deriving (Generic, Show)

instance FromJSON AttendInput

instance Injective AttendInput (UUID, Text, Text, Text, Text, Bool) where
  to AttendInput{eventId, email, firstName, lastName, status, plusOne} = (eventId, email, firstName, lastName, writeStatus status, plusOne)

instance Injective AttendInput (UUID, Text, Text, Bool) where
  to AttendInput{eventId, email, status, plusOne} = (eventId, email, writeStatus status, plusOne)

instance Injective AttendInput (UUID, Text) where
  to AttendInput{eventId, email} = (eventId, email)
