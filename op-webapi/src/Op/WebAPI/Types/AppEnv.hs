module Op.WebAPI.Types.AppEnv (
  AppEnv(..),
) where

import           Crypto.PubKey.RSA.Types    (PublicKey)
import           Data.Pool                  (Pool)
import           RIO

import qualified Op.Aws                     as Aws
import qualified Op.Cache                   as Cache
import qualified Op.Db                      as Db
import           Op.WebAPI.Types.HasHostUrl (HasHostUrl (..))

data AppEnv = AppEnv
  { connectionPool    :: Pool Db.Connection
  , hostUrl           :: Text
  , awsSnsPubKeyCache :: Cache.Cache Text PublicKey
  , logFunc           :: LogFunc
  , awsEnv            :: Aws.AwsEnv
  }

instance Db.HasDbConnection AppEnv where
  withDbConnection AppEnv{connectionPool} f = do
    Db.withDbConnection connectionPool f

instance Cache.HasCache Text PublicKey AppEnv where
  getCache = awsSnsPubKeyCache

instance HasLogFunc AppEnv where
  logFuncL = lens logFunc (\env f -> env { logFunc = f })

instance HasHostUrl AppEnv where
  getHostUrl = hostUrl

instance Aws.HasAwsEnv AppEnv where
  getAwsEnv = awsEnv
