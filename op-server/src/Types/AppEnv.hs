module Types.AppEnv (
  AppEnv(..),
  SmtpConfig(..),
) where

import           Data.Pool        (Pool)
import           Hasql.Connection (Connection)
import           Network.Socket   (PortNumber)
import qualified Op.Db            as Db

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

instance Db.HasConnectionPool AppEnv where
    getConnectionPool = connectionPool

instance Db.HasConnectionPool (Pool Connection) where
    getConnectionPool = id
