module Endpoints.Unsubscribe (unsubscribe) where

import           Control.Concurrent      (forkIO)
import           Control.Monad           (forM_, void, when)
import           Control.Monad.Except    (MonadError (throwError))
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Reader    (MonadReader, asks)
import           Data.Profunctor         (dimap, lmap)
import           Data.String.Interpolate (i)
import qualified Data.Text               as Text
import           Data.Types.Injective    (to)
import           Data.UUID               (UUID)
import           Email                   (CommentNotificationRecipient (..),
                                          sendEmailInvitation)
import           Endpoints.GetEvent      (getEvent)
import           Hasql.Connection        (Connection)
import           Hasql.Session           (CommandError (ResultError),
                                          QueryError (QueryError),
                                          ResultError (ServerError))
import qualified Hasql.Session           as Hasql
import           Hasql.Statement         (Statement)
import           Hasql.TH                (maybeStatement, resultlessStatement,
                                          singletonStatement, vectorStatement)
import           Servant                 (ServerError (errBody), err400, err404,
                                          err500)

import qualified Email
import           Types.AppEnv            (AppEnv (..), SmtpConfig (..))
import qualified Types.Attendee          as Attendee
import           Types.Attendee          (Attendee, writeStatus)
import           Types.Event             (Event)
import           Types.Unsubscribe       (UnsubscribeResult (..))

unsubscribe ::
  (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) =>
  UUID ->
  m UnsubscribeResult
unsubscribe unsubscribeId = do
  conn <- asks connection
  queryResult <- liftIO $ Hasql.run (Hasql.statement unsubscribeId statement) conn
  case queryResult of
    Right (Just (eventId, email, unsubscribedAt)) -> do
      event <- getEvent eventId
      pure $ UnsubscribeResult
        { unsubscribeResultEmail = email
        , unsubscribeResultUnsubscribedAt = unsubscribedAt
        , unsubscribeResultEvent = event
        }

    Right Nothing ->
      throwError err404 { errBody = [i|There's no attendee associated with the unsubscribe id: #{unsubscribeId}|] }

    Left err -> do
      liftIO $ print err
      throwError err500 { errBody = "Something went wrong" }
  where
    statement =
      [maybeStatement|
        update attendees set
          unsubscribed_at = coalesce(unsubscribed_at, now())
        where
          unsubscribe_id = $1::uuid
        returning
          event_id::uuid,
          email::text?,
          unsubscribed_at::timestamptz
      |]
