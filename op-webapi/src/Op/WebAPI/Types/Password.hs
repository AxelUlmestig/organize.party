module Op.WebAPI.Types.Password (Password (..)) where

import           Data.Aeson (FromJSON, ToJSON)
import           RIO

newtype Password = Password Text
  deriving Generic

instance Show Password where
  show _ = "********"

instance ToJSON Password
instance FromJSON Password
