module Op.Db (
  queryDbOr,
  printAndThrow500,
  createPool,
  beginTransactionOr,
  commitTransactionOr,
  rollbackTransactionOr,
  HasDbConnection(..),
  Hasql.statement,
) where

import           Control.Monad.Except     (MonadError (..))
import           Control.Monad.IO.Class   (MonadIO (liftIO))
import           Control.Monad.Reader     (MonadReader, ask)
import           Data.Pool                (Pool)
import qualified Data.Pool                as Pool
import           Hasql.Connection         (Connection)
import qualified Hasql.Connection         as Hasql
import           Hasql.Connection.Setting (Setting)
import qualified Hasql.Session            as Hasql
import           Servant                  (ServerError (..), err500)

class HasDbConnection a where
    withDbConnection :: MonadIO m => a -> (Connection -> IO b) -> m b
    -- getConnectionPool :: a -> Pool Connection

instance HasDbConnection (Pool Connection) where
    -- getConnectionPool = id
    withDbConnection connectionPool f = do
      liftIO $ Pool.withResource connectionPool f

instance HasDbConnection Connection where
    withDbConnection connection f = do
      liftIO $ f connection


queryDbOr
  :: (HasDbConnection env, MonadIO m, MonadReader env m)
  => (Hasql.SessionError -> m a)
  -> Hasql.Session a
  -> m a
queryDbOr onErr statement = do
  -- connectionPool <- asks getConnectionPool
  --
  -- eResult <- liftIO $ Pool.withResource connectionPool $ \connection ->
  --    Hasql.run statement connection
  env <- ask
  eResult <- withDbConnection env \connection -> do
    Hasql.run statement connection

  case eResult of
    Left err    -> onErr err
    Right event -> pure event

beginTransactionOr
  :: (HasDbConnection env, MonadIO m, MonadReader env m)
  => (Hasql.SessionError -> m ())
  -> m ()
beginTransactionOr onErr = do
  queryDbOr onErr (Hasql.sql "begin")

commitTransactionOr
  :: (HasDbConnection env, MonadIO m, MonadReader env m)
  => (Hasql.SessionError -> m ())
  -> m ()
commitTransactionOr onErr = do
  queryDbOr onErr (Hasql.sql "commit")

rollbackTransactionOr
  :: (HasDbConnection env, MonadIO m, MonadReader env m)
  => (Hasql.SessionError -> m ())
  -> m ()
rollbackTransactionOr onErr = do
  queryDbOr onErr (Hasql.sql "rollback")

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

