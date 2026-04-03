module Op.WebAPI.Types.CommentInput (CommentInput(..), emailToLowerCase) where

import           Data.Aeson (FromJSON)
import           Data.Text  (toLower)
import           Data.UUID  (UUID)
import           RIO


data CommentInput
  = CommentInput
    { eventId                    :: UUID
    , email                      :: Text
    , name                       :: Text
    , comment                    :: Text
    , forceNotificationOnComment :: Bool
    }
    deriving (Eq, Generic, Show)

instance FromJSON CommentInput

emailToLowerCase :: CommentInput -> CommentInput
emailToLowerCase commentInput = commentInput { email = toLower commentInput.email }
