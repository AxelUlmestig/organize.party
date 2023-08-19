module Endpoints.Comment (addComment) where

import           Control.Concurrent     (forkIO)
import           Control.Monad          (void, when)
import           Control.Monad.Except   (MonadError (throwError))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Profunctor        (dimap, lmap)
import           Data.Text              (pack)
import           Data.Types.Injective   (to)
import           Data.UUID              (UUID)
import           Email                  (sendEmailInvitation)
import           Endpoints.GetEvent     (getEvent)
import           Hasql.Connection       (Connection)
import           Hasql.Session          (CommandError (ResultError),
                                         QueryError (QueryError),
                                         ResultError (ServerError))
import qualified Hasql.Session          as Hasql
import           Hasql.Statement        (Statement)
import           Hasql.TH               (maybeStatement, resultlessStatement,
                                         singletonStatement)
import           Servant                (ServerError (errBody), err400, err404,
                                         err500)

import qualified Email
import           Types.AppEnv           (AppEnv (..), SmtpConfig (..))
import qualified Types.Attendee         as Attendee
import           Types.Attendee         (Attendee, writeStatus)
import qualified Types.CommentInput     as VP
import           Types.CommentInput     (CommentInput (..))
import           Types.Event            (Event)


addComment :: (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) => UUID -> CommentInput -> m Event
addComment eventId commentInput@CommentInput { eventId = bodyEventId } = do
  when (eventId /= bodyEventId) $
    throwError err400 { errBody = "Event id in the URL has to be the same as the event id in the body" }

  conn <- asks connection
  queryResult <- liftIO $ Hasql.run (Hasql.statement commentInput insertCommentStatement) conn
  case queryResult of
    Right () -> do
      getEvent eventId
    Left err -> do
      liftIO $ print err
      case err of
        QueryError _ _ (ResultError (ServerError "23503" _ _ _ _))  -> throwError err404 { errBody = "Event not found" }
        QueryError _ _ (ResultError (ServerError "23514" _ _ _ _))  -> throwError err400 { errBody = "Comment can't be empty" }
        _                                                           -> throwError err500 { errBody = "Something went wrong" }

insertCommentStatement :: Statement CommentInput ()
insertCommentStatement =
  lmap to
    [resultlessStatement|
      insert into comments (event_id, email, name, comment)
      values ($1::uuid, $2::text, $3::text, $4::text)
    |]
