{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.Comment (addComment) where

import           Control.Monad.Except         (MonadError (throwError))
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Reader         (MonadReader, asks)
import           Data.String.Interpolate      (i)
import           Data.UUID                    (UUID)
{-
import           Hasql.Errors                 (CommandError (ResultError),
                                               ResultError (ServerError),
                                               SessionError (QueryError))
                                            -}
import qualified Data.Text                    as Text
import           RIO                          (when)
import           Servant                      (ServerError (errBody), err400,
                                               err500)

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
      liftIO $ putStrLn [i|Something went wrong when adding comment: #{err}|]
      case err of
        -- TODO: look up the error types in the new Hasql API
        {-
        QueryError _ _ (ResultError (ServerError "23503" _ _ _ _))  -> throwError err404 { errBody = "Event not found" }
        QueryError _ _ (ResultError (ServerError "23514" _ _ _ _))  -> throwError err400 { errBody = "Comment can't be empty" }
        -}
        _                                                           -> throwError err500 { errBody = "Something went wrong" }

