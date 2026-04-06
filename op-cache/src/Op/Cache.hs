module Op.Cache (
  Cache,
  initCache,
  getCached,
  getCachedM,
  HasCache(..)
) where

import qualified Control.Concurrent.STM as STM
import           RIO
import qualified RIO.HashMap            as HM

newtype Cache k v = Cache (STM.TVar (HM.HashMap k v))

class HasCache k v a where
  getCache :: a -> Cache k v

instance HasCache k v (Cache k v) where
  getCache = id

initCache :: MonadIO m => m (Cache k v)
initCache = liftIO do
  hashMapTVar <- STM.atomically $ STM.newTVar HM.empty
  pure $ Cache hashMapTVar

getCached :: (MonadIO m, Hashable k) => Cache k v -> k -> m v -> m v
getCached (Cache tvar) key getValue = do
  cachedValues <- liftIO $ STM.atomically $ STM.readTVar tvar
  case HM.lookup key cachedValues of
    Just value -> pure value
    Nothing -> do
      value <- getValue
      liftIO $ STM.atomically $ STM.modifyTVar' tvar (HM.insert key value)
      pure value

getCachedM :: (MonadIO m, MonadReader env m, Hashable k, HasCache k v env)
  => k
  -> m v
  -> m v
getCachedM key getValue = do
  cache <- asks getCache
  getCached cache key getValue
