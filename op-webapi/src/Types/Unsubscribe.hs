module Types.Unsubscribe (UnsubscribeResult(..)) where

import           Data.Aeson
import           Data.Text    (Text, pack, toLower)
import           Data.Time    (UTCTime)
import           GHC.Generics (Generic)
import           Types.Event  (Event (..))

data UnsubscribeResult
  = UnsubscribeResult
    { unsubscribeResultEmail          :: Maybe Text
    , unsubscribeResultUnsubscribedAt :: UTCTime
    , unsubscribeResultEvent          :: Event
    }
    deriving (Eq, Generic, Show)

instance ToJSON UnsubscribeResult where
  toJSON UnsubscribeResult{unsubscribeResultEmail, unsubscribeResultUnsubscribedAt, unsubscribeResultEvent} =
    object
      [ "email"           .= unsubscribeResultEmail
      , "unsubscribedAt"  .= unsubscribeResultUnsubscribedAt
      , "event"           .= toJSON unsubscribeResultEvent
      ]

