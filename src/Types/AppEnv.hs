module Types.AppEnv (AppEnv(..), SmtpConfig(..)) where

import           Hasql.Connection (Connection)
import           Network.Socket   (PortNumber)

data AppEnv = AppEnv
  { connection :: Connection
  , smtpConfig :: SmtpConfig
  }


data SmtpConfig = SmtpConfig
  { server   :: String
  , port     :: PortNumber
  , login    :: String
  , password :: String
  }

