module Op.WebAPI.Types.AttendInput (AttendInput(..)) where

import           Data.Aeson               (FromJSON)
import           Data.Types.Isomorphic    (Injective (to))
import           Data.UUID                (UUID)
import           RIO

import           Op.WebAPI.Types.Attendee (AttendeeStatus (..), writeStatus)

data AttendInput = AttendInput
                   { eventId               :: UUID
                   , email                 :: Text
                   , name                  :: Text
                   , status                :: AttendeeStatus
                   , plusOne               :: Bool
                   , getNotifiedOnComments :: Bool
                   }
                   deriving (Generic, Show)

instance FromJSON AttendInput

instance Injective AttendInput (UUID, Text, Text, Text, Bool, Bool) where
  to AttendInput{eventId, email, name, status, plusOne, getNotifiedOnComments} = (eventId, email, name, writeStatus status, plusOne, getNotifiedOnComments)

instance Injective AttendInput (UUID, Text, Text, Bool) where
  to AttendInput{eventId, email, status, plusOne} = (eventId, email, writeStatus status, plusOne)

instance Injective AttendInput (UUID, Text) where
  to AttendInput{eventId, email} = (eventId, email)
