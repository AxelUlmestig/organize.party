module Types.CommentInput (CommentInput(..)) where

import           Data.Aeson            (FromJSON)
import           Data.Int              (Int64)
import           Data.Text             (Text, pack)
import           Data.Time             (UTCTime)
import           Data.Types.Isomorphic (Injective (to))
import           Data.UUID             (UUID)
import           GHC.Generics          (Generic)


data CommentInput
  = CommentInput
    { eventId :: UUID
    , email   :: Text
    , name    :: Text
    , comment :: Text
    }
    deriving (Eq, Generic, Show)

instance FromJSON CommentInput

instance Injective CommentInput (UUID, Text, Text, Text) where
  to CommentInput{eventId, email, name, comment} = (eventId, email, name, comment)

