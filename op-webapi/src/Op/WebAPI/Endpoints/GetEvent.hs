{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.GetEvent (
  getEvent,
  maybeGetEvent
) where

import           Control.Monad.Except    (MonadError (throwError))
import qualified Data.Aeson              as Aeson
import           Data.String.Interpolate (i)
import           Data.UUID               (UUID)
import           RIO
import           Servant                 (ServerError (..), err404, err500)

import qualified Op.Db                   as Db
import           Op.WebAPI.Types.AppEnv
import           Op.WebAPI.Types.Event   (Event)

getEvent :: (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) => UUID -> m Event
getEvent eventId = do
  mEvent <- maybeGetEvent eventId

  case mEvent of
    Just event -> pure event
    Nothing    -> throwError err404 { errBody = "Event not found" }

maybeGetEvent :: (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) => UUID -> m (Maybe Event)
maybeGetEvent eventId = do
  mJson <- do
    Db.queryDbOr Db.printAndThrow500 do
      Db.statement
        eventId
        [Db.singletonStatement|
          select get_event_json($1::uuid)::jsonb?
        |]

  case Aeson.fromJSON <$> mJson of
    Nothing -> pure Nothing
    Just (Aeson.Success event) -> pure $ Just event
    Just (Aeson.Error err) -> do
      logError [i|Could not parse event json: #{Aeson.encode <$> mJson}\n\nerr: #{err}|]
      throwError err500 { errBody = "Something went wrong" }
