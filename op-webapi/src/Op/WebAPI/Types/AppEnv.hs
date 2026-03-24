module Op.WebAPI.Types.AppEnv (
  AppEnv(..),
) where

import           Crypto.PubKey.RSA.Types (PublicKey)
import           Data.Pool               (Pool)
import           RIO

import qualified Op.Cache                as Cache
import qualified Op.Db                   as Db

data AppEnv = AppEnv
  { connectionPool    :: Pool Db.Connection
  , hostUrl           :: String
  , awsSnsPubKeyCache :: Cache.Cache Text PublicKey
  , logFunc           :: LogFunc
  }

instance Db.HasDbConnection AppEnv where
  withDbConnection AppEnv{connectionPool} f = do
    Db.withDbConnection connectionPool f

instance Cache.HasCache Text PublicKey AppEnv where
  getCache = awsSnsPubKeyCache

instance HasLogFunc AppEnv where
  logFuncL = lens logFunc (\env f -> env { logFunc = f })

