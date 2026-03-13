{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.Comment (addComment) where

import           Control.Monad                (forM_, when)
import           Control.Monad.Except         (MonadError (throwError))
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Reader         (MonadReader, asks)
import           Data.Profunctor              (lmap)
import           Data.String.Interpolate      (i)
import           Data.Types.Injective         (to)
import           Data.UUID                    (UUID)
{-
import           Hasql.Errors                 (CommandError (ResultError),
                                               ResultError (ServerError),
                                               SessionError (QueryError))
                                            -}
import qualified Data.Text                    as Text
import qualified Hasql.Session                as Hasql
import           Hasql.Statement              (Statement)
import           RIO                          (Text)
import           Servant                      (ServerError (errBody), err400,
                                               err404, err500)

import qualified Op.Db                        as Db
import           Op.WebAPI.Email              (CommentNotificationRecipient (..),
                                               EmailData (..))
import qualified Op.WebAPI.Email              as Email
import           Op.WebAPI.Endpoints.GetEvent (getEvent)
import           Op.WebAPI.Types.AppEnv       (AppEnv (..))
import           Op.WebAPI.Types.CommentInput (CommentInput (..))
import qualified Op.WebAPI.Types.CommentInput as CommentInput
import           Op.WebAPI.Types.Event        (Event)


addComment :: (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) => UUID -> CommentInput -> m Event
addComment eventId commentInput' = do
  let commentInput = CommentInput.emailToLowerCase commentInput'

  when (eventId /= commentInput.eventId) $
    throwError err400 { errBody = "Event id in the URL has to be the same as the event id in the body" }

  do
    emailHostUrl <- asks (Text.pack . hostUrl)
    Db.queryDbOr handleErr (Hasql.statement (emailHostUrl, commentInput) insertCommentStatement)

  sendEmailUpdate commentInput
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

insertCommentStatement :: Statement (Text, CommentInput) ()
insertCommentStatement =
  lmap to'
    [Db.resultlessStatement|
      insert into comments (comment, force_notification_on_comment, attendee_id, event_id)
      select
        $5::text as comment,
        $6::bool as force_notification_on_comment,
        id,
        event_id
      from
        add_attendee_data(
          host_url_ => $1::text,
          event_id_ => $2::uuid,
          email_ => $3::text,
          name_ => $4::text
        )
    |]
  where
    to' (hostUrl, CommentInput{eventId, email, name, comment, forceNotificationOnComment}) =
      (hostUrl, eventId, email, name, comment, forceNotificationOnComment)


sendEmailUpdate ::
  (MonadError ServerError m,
  MonadReader AppEnv m,
  MonadIO m) =>
  CommentInput ->
  m ()
sendEmailUpdate commentInput = do
  emailHostUrl <- asks hostUrl

  let toEmailRecipient (email, recipientName, eventTitle, forcePush, unsubscribeId) = (EmailData{..}, CommentNotificationRecipient{..})
  let statement = fmap toEmailRecipient <$>
        [Db.vectorStatement|
          select
            attendees.email::text,
            attendee_data.name::text,
            event_data.title::text,
            ($3::bool and not attendee_data.get_notified_on_comments)::bool as forced,
            unsubscribe_id::uuid
          from event_data
          join attendees
            on attendees.event_id = event_data.id
            and attendees.deleted_at is null
          join attendee_data
            on attendee_data.attendee_id = attendees.id
            and attendee_data.superseded_at is null
          where
            event_data.id = $1::uuid
            and event_data.superseded_at is null
            and attendees.email <> $2::text
            and unsubscribed_at is null
            and (
              attendee_data.get_notified_on_comments
              or $3::bool
            )
        |]

  subscribers <- Db.queryDbOr Db.printAndThrow500 (Hasql.statement (commentInput.eventId, commentInput.email, commentInput.forceNotificationOnComment) statement)

  forM_ subscribers $ \(emailData, subscriber) -> do
    Email.sendCommentNotifications Db.printAndThrow500 emailData commentInput subscriber

