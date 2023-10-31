module Endpoints.Comment (addComment) where

import           Control.Concurrent     (forkIO)
import           Control.Monad          (forM_, void, when)
import           Control.Monad.Except   (MonadError (throwError))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Profunctor        (dimap, lmap)
import qualified Data.Text              as Text
import           Data.Types.Injective   (to)
import           Data.UUID              (UUID)
import           Email                  (CommentNotificationRecipient (..),
                                         sendEmailInvitation)
import           Endpoints.GetEvent     (getEvent)
import           Hasql.Connection       (Connection)
import           Hasql.Session          (CommandError (ResultError),
                                         QueryError (QueryError),
                                         ResultError (ServerError))
import qualified Hasql.Session          as Hasql
import           Hasql.Statement        (Statement)
import           Hasql.TH               (maybeStatement, resultlessStatement,
                                         singletonStatement, vectorStatement)
import           Servant                (ServerError (errBody), err400, err404,
                                         err500)

import qualified Email
import           Types.AppEnv           (AppEnv (..), SmtpConfig (..))
import qualified Types.Attendee         as Attendee
import           Types.Attendee         (Attendee, writeStatus)
import qualified Types.CommentInput     as CommentInput
import           Types.CommentInput     (CommentInput (..))
import           Types.Event            (Event)


addComment :: (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) => UUID -> CommentInput -> m Event
addComment eventId commentInput' = do
  let commentInput = CommentInput.emailToLowerCase commentInput'

  when (eventId /= commentInput.eventId) $
    throwError err400 { errBody = "Event id in the URL has to be the same as the event id in the body" }

  conn <- asks connection
  queryResult <- liftIO $ Hasql.run (Hasql.statement commentInput insertCommentStatement) conn
  case queryResult of
    Right () -> do
      sendEmailUpdate commentInput
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
      with inserted_commenter as (
        insert into commenters (event_id, email, name, gravatar_url)
        values ($1::uuid, $2::text, $3::text, 'https://www.gravatar.com/avatar/' || md5($2::text))
        on conflict (event_id, email)
        do update set
          name = $3::text
        returning *
      )

      insert into comments (event_id, email, comment, force_notification_on_comment)
      select event_id, email, $4::text, $5::bool
      from inserted_commenter
    |]

sendEmailUpdate commentInput = do
  conn <- asks connection
  eSubscribers <- liftIO $ Hasql.run (Hasql.statement (commentInput.eventId, commentInput.email, commentInput.forceNotificationOnComment) statement) conn

  case eSubscribers of
    Left err -> do
      liftIO $ print err
      throwError err500 { errBody = "Something went wrong" }
    Right subscribers -> do
      smtpConf <- asks smtpConfig
      liftIO $ forM_ subscribers $ \subscriber -> do
        forkIO $ Email.sendCommentNotifications smtpConf commentInput subscriber
  where
    statement = fmap toEmailRecipient <$>
      [vectorStatement|
        with distinct_comments as (
          select
            email,
            name,
            event_id
          from commenters
          where
            event_id = $1::uuid
        )

        select
          coalesce(attendees.email, distinct_comments.email)::text,
          coalesce(attendees.name, distinct_comments.name)::text,
          event_data.title::text,
          (not coalesce(attendees.get_notified_on_comments, false) and $3::bool)::bool as forced
        from event_data
        left join attendees
          on attendees.event_id = event_data.id
          and attendees.superseded_at is null
          and attendees.email <> $2::text
        left join distinct_comments
          on distinct_comments.event_id = event_data.id
          and distinct_comments.email <> $2::text
          and (
            attendees.email is null
            or attendees.email = distinct_comments.email
          )
        where
          event_data.id = $1::uuid
          and event_data.superseded_at is null
          and (
            coalesce(attendees.get_notified_on_comments, false)
            or $3::bool
          )
      |]
      where
        toEmailRecipient (email, recipientName, eventTitle, forcePush) = CommentNotificationRecipient{..}
