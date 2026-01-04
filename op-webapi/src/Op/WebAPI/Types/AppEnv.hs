module Op.WebAPI.Types.AppEnv (
  AppEnv(..),
  SmtpConfig(..),
) where

import           Data.Pool        (Pool)
import           Hasql.Connection (Connection)
import           Network.Socket   (PortNumber)
import qualified Op.Db            as Db

data AppEnv = AppEnv
  { connectionPool :: Pool Connection
  , hostUrl        :: String
  }


data SmtpConfig = SmtpConfig
  { server   :: String
  , port     :: PortNumber
  , login    :: String
  , password :: String
  }

instance Db.HasDbConnection AppEnv where
    withDbConnection AppEnv{connectionPool} f = do
      Db.withDbConnection connectionPool f

