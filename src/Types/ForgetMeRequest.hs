module Types.ForgetMeRequest (
  InitForgetMeInput(..),
  InitForgetMeResult(..),
  ForgetMeRequest(..),
) where

import           Data.Aeson
import           Data.Int              (Int64)
import           Data.Text             (Text, pack, toLower)
import           Data.Time             (UTCTime)
import           Data.Types.Isomorphic (Injective (to))
import           Data.UUID             (UUID)
import           GHC.Generics          (Generic)


newtype InitForgetMeInput
  = InitForgetMeInput
    { email :: Text
    }
    deriving (Eq, Generic, Show)

instance FromJSON InitForgetMeInput

instance Injective InitForgetMeInput Text where
  to InitForgetMeInput{email} = email

newtype InitForgetMeResult
  = InitForgetMeResult
    { initForgetMeResultEmail :: Text
    }
    deriving (Eq, Generic, Show)

instance ToJSON InitForgetMeResult where
  toJSON InitForgetMeResult{initForgetMeResultEmail} =
    object
      [ "email" .= initForgetMeResultEmail
      ]

data ForgetMeRequest
  = ForgetMeRequest
    { forgetMeRequestId        :: UUID
    , forgetMeRequestEmail     :: Maybe Text
    , forgetMeRequestDeletedAt :: Maybe UTCTime
    }
    deriving (Eq, Generic, Show)

instance ToJSON ForgetMeRequest where
  toJSON ForgetMeRequest{forgetMeRequestId, forgetMeRequestEmail, forgetMeRequestDeletedAt} =
    object
      [ "id"        .= forgetMeRequestId
      , "email"     .= forgetMeRequestEmail
      , "deletedAt" .= forgetMeRequestDeletedAt
      ]

