module Types.CommentInput (CommentInput(..), emailToLowerCase) where

import           Data.Aeson            (FromJSON)
import           Data.Int              (Int64)
import           Data.Text             (Text, pack, toLower)
import           Data.Time             (UTCTime)
import           Data.Types.Isomorphic (Injective (to))
import           Data.UUID             (UUID)
import           GHC.Generics          (Generic)


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

instance Injective CommentInput (UUID, Text, Text, Text, Bool) where
  to CommentInput{eventId, email, name, comment, forceNotificationOnComment} =
    (eventId, email, name, comment, forceNotificationOnComment)

emailToLowerCase :: CommentInput -> CommentInput
emailToLowerCase commentInput = commentInput { email = toLower commentInput.email }
