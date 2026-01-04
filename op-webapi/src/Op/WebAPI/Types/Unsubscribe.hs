module Op.WebAPI.Types.Unsubscribe (UnsubscribeResult(..)) where

import           Data.Aeson
import           Data.Text             (Text)
import           Data.Time             (UTCTime)
import           GHC.Generics          (Generic)
import           Op.WebAPI.Types.Event (Event (..))

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

