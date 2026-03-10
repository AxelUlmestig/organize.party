module Op.WebAPI.Types.AppEnv (
  AppEnv(..),
  SmtpConfig(..),
) where

import           Crypto.PubKey.RSA.Types (PublicKey)
import           Data.Pool               (Pool)
import           Hasql.Connection        (Connection)
import           Network.Socket          (PortNumber)
import           RIO                     (Text)

import qualified Op.Cache                as Cache
import qualified Op.Db                   as Db

data AppEnv = AppEnv
  { connectionPool    :: Pool Connection
  , hostUrl           :: String
  , awsSnsPubKeyCache :: Cache.Cache Text PublicKey
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

instance Cache.HasCache Text PublicKey AppEnv where
  getCache = awsSnsPubKeyCache
