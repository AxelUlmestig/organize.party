module Types.AppEnv (
  AppEnv(..),
  SmtpConfig(..),
  HasConnectionPool(..),
) where

import           Data.Pool        (Pool)
import           Hasql.Connection (Connection)
import           Network.Socket   (PortNumber)

data AppEnv = AppEnv
  { connectionPool :: Pool Connection
  , smtpConfig     :: SmtpConfig
  , hostUrl        :: String
  }


data SmtpConfig = SmtpConfig
  { server   :: String
  , port     :: PortNumber
  , login    :: String
  , password :: String
  }

class HasConnectionPool a where
    getConnectionPool :: a -> Pool Connection

instance HasConnectionPool AppEnv where
    getConnectionPool = connectionPool

instance HasConnectionPool (Pool Connection) where
    getConnectionPool = id
