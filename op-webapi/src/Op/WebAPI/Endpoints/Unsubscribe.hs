{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.Unsubscribe (unsubscribe) where

import           Control.Monad.Except         (MonadError (throwError))
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Reader         (MonadReader)
import           Data.String.Interpolate      (i)
import           Data.UUID                    (UUID)
import qualified Hasql.Session                as Hasql
import           Servant                      (ServerError (errBody), err404)

import qualified Op.Db                        as Db
import           Op.WebAPI.Endpoints.GetEvent (getEvent)
import           Op.WebAPI.Types.AppEnv       (AppEnv (..))
import           Op.WebAPI.Types.Unsubscribe  (UnsubscribeResult (..))

unsubscribe ::
  (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) =>
  UUID ->
  m UnsubscribeResult
unsubscribe unsubscribeId = do
  queryResult <- Db.queryDbOr Db.printAndThrow500 (Hasql.statement unsubscribeId statement)
  case queryResult of
    Just (eventId, email, unsubscribedAt) -> do
      event <- getEvent eventId
      pure $ UnsubscribeResult
        { unsubscribeResultEmail = email
        , unsubscribeResultUnsubscribedAt = unsubscribedAt
        , unsubscribeResultEvent = event
        }

    Nothing ->
      throwError err404 { errBody = [i|There's no attendee associated with the unsubscribe id: #{unsubscribeId}|] }
  where
    statement =
      [Db.maybeStatement|
        update attendees set
          unsubscribed_at = coalesce(unsubscribed_at, now())
        where
          unsubscribe_id = $1::uuid
        returning
          event_id::uuid,
          email::text?,
          unsubscribed_at::timestamptz
      |]
