module Util.Db (
  queryDbOr,
  printAndThrow500,
  createPool,
) where

import           Control.Exception        (throwIO)
import           Control.Monad.Except     (MonadError (..))
import           Control.Monad.IO.Class   (MonadIO (liftIO))
import           Control.Monad.Reader     (MonadReader, asks)
import qualified Data.Pool                as Pool
import qualified Data.Text                as Text
import qualified Hasql.Connection         as Hasql
import           Hasql.Connection.Setting (Setting)
import qualified Hasql.Session            as Hasql
import           Hasql.Statement          (Statement)
import           Hasql.TH                 (resultlessStatement)
import           Servant                  (ServerError (..), err500)

import           Types.AppEnv

queryDbOr
  :: (HasConnectionPool cp, MonadIO m, MonadReader cp m)
  => (Hasql.SessionError -> m a)
  -> Hasql.Session a
  -> m a
queryDbOr onErr statement = do
  connectionPool <- asks getConnectionPool

  eResult <- liftIO $ Pool.withResource connectionPool $ \connection ->
      Hasql.run statement connection

  case eResult of
    Left err    -> onErr err
    Right event -> pure event

printAndThrow500
  :: (MonadError ServerError m, MonadIO m)
  => Hasql.SessionError
  -> m a
printAndThrow500 err = do
  liftIO $ print err
  throwError err500 { errBody = "Something went wrong" }

createPool :: (MonadIO m) => [Setting] -> m (Pool.Pool Hasql.Connection)
createPool dbConnectionSettings = do
  let poolConfig = Pool.defaultPoolConfig
        (Hasql.acquire dbConnectionSettings >>= either (error . show) pure) -- TODO: properly retry here
        Hasql.release
        60
        10
  liftIO $ Pool.newPool poolConfig

