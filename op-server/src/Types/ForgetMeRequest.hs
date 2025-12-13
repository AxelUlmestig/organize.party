module Types.ForgetMeRequest (
  InitForgetMeInput(..),
  InitForgetMeResult(..),
  ForgetMeRequest(..),
  ExecuteForgetMeResult(..),
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

data InitForgetMeResult
  = InitForgetMeResult
    { initForgetMeResultId    :: UUID
    , initForgetMeResultEmail :: Text
    }
    deriving (Eq, Generic, Show)

instance ToJSON InitForgetMeResult where
  toJSON InitForgetMeResult{initForgetMeResultEmail} =
    object
      [ "id"        .= (Nothing :: Maybe UUID)
      , "email"     .= initForgetMeResultEmail
      , "deletedAt" .= (Nothing :: Maybe UTCTime)
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

data ExecuteForgetMeResult
  = ExecuteForgetMeResult
    { forgetMeResultId        :: UUID
    , forgetMeResultDeletedAt :: UTCTime
    }
    deriving (Eq, Generic, Show)

instance ToJSON ExecuteForgetMeResult where
  toJSON ExecuteForgetMeResult{forgetMeResultId, forgetMeResultDeletedAt} =
    object
      [ "id"        .= forgetMeResultId
      , "email"     .= (Nothing :: Maybe Text)
      , "deletedAt" .= forgetMeResultDeletedAt
      ]

