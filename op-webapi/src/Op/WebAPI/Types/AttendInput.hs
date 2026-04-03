module Op.WebAPI.Types.AttendInput (AttendInput(..)) where

import           Data.Aeson               (FromJSON)
import           Data.UUID                (UUID)
import           RIO

import           Op.WebAPI.Types.Attendee (AttendeeStatus (..))

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

