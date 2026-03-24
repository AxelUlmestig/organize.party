{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.Comment (addComment) where

import           Control.Monad.Except         (MonadError (throwError))
import           Data.String.Interpolate      (i)
import qualified Data.Text                    as Text
import           Data.UUID                    (UUID)
import           RIO
import           Servant                      (ServerError (errBody), err400,
                                               err404, err500)

import qualified Op.Db                        as Db
import           Op.WebAPI.Endpoints.GetEvent (getEvent)
import           Op.WebAPI.Types.AppEnv       (AppEnv (..))
import           Op.WebAPI.Types.CommentInput (CommentInput (..))
import           Op.WebAPI.Types.Event        (Event)


addComment :: (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) => UUID -> CommentInput -> m Event
addComment urlEventId CommentInput{..} = do
  when (urlEventId /= eventId) $
    throwError err400 { errBody = "Event id in the URL has to be the same as the event id in the body" }

  emailHostUrl <- asks (Text.pack . hostUrl)
  Db.queryDbOr handleErr do
    Db.statement
      (emailHostUrl, eventId, email, name, comment, forceNotificationOnComment)
      [Db.resultlessStatement|
        select add_comment(
          host_url_ => $1::text,
          event_id_ => $2::uuid,
          email_ => $3::text,
          name_ => $4::text,
          comment_ => $5::text,
          force_notification_on_comment_ => $6::bool
        )::text
      |]

  getEvent eventId

  where
    handleErr err = do
      case err of
        Db.StatementSessionError _ _ _ _ _ (Db.ServerStatementError (Db.ServerError "23514" _ _ _ _)) ->
          throwError err400 { errBody = "Comment can't be empty" }
        Db.StatementSessionError _ _ _ _ _ (Db.ServerStatementError (Db.ServerError "23503" _ _ _ _)) ->
          throwError err404 { errBody = "Event not found" }
        _ -> do
          logError [i|Something went wrong when adding comment: #{err}|]
          throwError err500 { errBody = "Something went wrong" }

