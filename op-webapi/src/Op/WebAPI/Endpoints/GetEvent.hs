{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.GetEvent (
  getEvent,
  maybeGetEvent,
  getAttendees,
  getAttendeesStatement,
  getCommentsStatement
) where

import           Control.Monad.Except   (MonadError (throwError))
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader)
import           Data.Types.Injective   (to)
import           Data.UUID              (UUID)
import qualified Data.Vector            as Vector
import qualified Hasql.Session          as Hasql
import           Hasql.Statement        (Statement)
import           Servant                (ServerError (..), err404)

import qualified Op.Db                  as Db
import           Op.WebAPI.Types.AppEnv
import           Op.WebAPI.Types.Event  (Attendee, Comment (..), Event (..))

getEvent :: (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) => UUID -> m Event
getEvent eventId = do
  mEvent <- maybeGetEvent eventId
  case mEvent of
    Just event -> pure event
    Nothing    -> throwError err404 { errBody = "Event not found" }

maybeGetEvent :: (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) => UUID -> m (Maybe Event)
maybeGetEvent eventId = do
    let statement = do
            mEvent <- Hasql.statement eventId getEventStatement
            case mEvent of
              Nothing    -> pure Nothing
              Just event -> do
                attendees <- Hasql.statement event.id getAttendeesStatement
                comments <- Hasql.statement event.id getCommentsStatement
                pure $ Just event { attendees, comments }

    mEvent <- Db.queryDbOr Db.printAndThrow500 statement

    traverse getAttendees mEvent


getEventStatement :: Statement UUID (Maybe Event)
getEventStatement =
  fmap to <$>
    [Db.maybeStatement|
      select
         event_data.id::uuid,
         event_data.title::text,
         event_data.description::text,
         event_data.time_start::timestamptz,
         event_data.time_end::timestamptz?,
         event_data.location::text,
         event_data.location_google_maps_link::text?,
         events.created_at::timestamptz,
         event_data.created_at::timestamptz
      from event_data
      join events
        on events.id = event_data.id
      where
        event_data.id = $1::uuid
        and event_data.superseded_at is null
    |]

getAttendees :: (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) => Event -> m Event
getAttendees event@Event{Op.WebAPI.Types.Event.id} = do
  attendees <- Db.queryDbOr Db.printAndThrow500 (Hasql.statement id getAttendeesStatement)
  return $ event { attendees = attendees }


getAttendeesStatement :: Statement UUID [Attendee]
getAttendeesStatement =
  fmap to . Vector.toList <$>
    [Db.vectorStatement|
      select
        attendee_data.name::text,
        attendee_data.status::text,
        attendee_data.plus_one::bool
      from attendees
      join attendee_data
        on attendee_data.attendee_id = attendees.id
        and attendee_data.superseded_at is null
        and attendee_data.status is not null
      where
        attendees.event_id = $1::uuid
        and attendees.deleted_at is null
      order by
        attendee_data.status,
        attendee_data.rsvp_at desc
    |]

getCommentsStatement :: Statement UUID [Comment]
getCommentsStatement =
  fmap to . Vector.toList <$>
    [Db.vectorStatement|
      select
        attendee_data.name::text,
        comments.comment::text,
        comments.created_at::timestamptz,
        attendees.gravatar_url::text?
      from attendees
      join comments
        on comments.attendee_id = attendees.id
      join attendee_data
        on attendee_data.attendee_id = attendees.id
        and attendee_data.superseded_at is null
      where
        attendees.event_id = $1::uuid
    |]
