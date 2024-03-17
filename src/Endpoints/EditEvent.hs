module Endpoints.EditEvent (editEvent) where

import           Control.Concurrent      (forkIO)
import           Control.Monad           (forM_)
import           Control.Monad.Except    (MonadError (..))
import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Control.Monad.Reader    (MonadReader, asks)
import           Data.Profunctor         (dimap)
import           Data.String.Interpolate (i)
import           Data.Text               (Text)
import           Data.Types.Isomorphic   (to)
import           Data.UUID               (UUID)
import qualified Hasql.Session           as Hasql
import           Hasql.Statement         (Statement)
import           Hasql.TH                (maybeStatement, resultlessStatement,
                                          singletonStatement, vectorStatement)
import           Servant                 (ServerError (..), err403, err404,
                                          err500)

import           Email                   (sendEventUpdateEmail)
import           Endpoints.GetEvent      (getAttendeesStatement,
                                          getCommentsStatement)
import           Types.AppEnv            (AppEnv (..), SmtpConfig (..),
                                          connection)
import           Types.CreateEventInput
import qualified Types.Event             as Event
import           Types.Event             (Event)

data EditResult
  = Success Event
  | Forbidden
  | NotFound
  deriving (Eq)

editEvent :: (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) => UUID -> CreateEventInput -> m Event
editEvent eventId input = do
  conn <- asks connection
  eEvent <- liftIO $ Hasql.run (session (eventId, input)) conn
  case eEvent of
    Left err -> do
      liftIO $ print err
      throwError err500 { errBody = "Something went wrong" }
    Right Forbidden -> throwError err403 { errBody = "Password didn't match" }
    Right NotFound -> throwError err404 { errBody = "Event not found" }
    Right (Success event) -> do
      smtpConf <- asks smtpConfig
      sendEmailUpdate event
      pure event

session :: (UUID, CreateEventInput) -> Hasql.Session EditResult
session (eventId, input) = do
  editValidityStatus <- Hasql.statement (eventId, Types.CreateEventInput.password input) existsStatement
  case editValidityStatus of
    EventNotFound -> pure NotFound
    IncorrectPassword -> pure Forbidden
    CorrectPassword -> do
      event <- Hasql.statement (eventId, input) updateEventDataStatement
      attendees <- Hasql.statement event.id getAttendeesStatement
      comments <- Hasql.statement event.id getCommentsStatement
      pure $ Success $ event { Event.attendees = attendees, Event.comments = comments }


data EditValidityStatus
  = CorrectPassword
  | IncorrectPassword
  | EventNotFound
  deriving (Eq)


existsStatement :: Statement (UUID, Text) EditValidityStatus
existsStatement =
  parse <$>
    [maybeStatement|
      select
        (password_hash = digest($2::text|| password_salt, 'sha256')::text)::bool
      from events
      where id = $1::uuid
    |]
  where
    parse x = case x of
      Nothing    -> EventNotFound
      Just True  -> CorrectPassword
      Just False -> IncorrectPassword


updateEventDataStatement :: Statement (UUID, CreateEventInput) Event
updateEventDataStatement =
  dimap f to
  [singletonStatement|
    with
      previous_event_data as (
        update event_data
        set superseded_at = now()
        where
          id = $1::uuid
          and superseded_at is null
        returning *
      ),

      inserted_event_data as (
        insert into event_data (
          id,
          title,
          description,
          time_start,
          time_end,
          location,
          location_google_maps_link
        )
        select
          previous_event_data.id,
          $2::text,
          $3::text,
          $4::timestamptz,
          $5::timestamptz?,
          $6::text,
          $7::text?
        from previous_event_data
        returning *
      )

    select
      inserted_event_data.id::uuid,
      inserted_event_data.title::text,
      inserted_event_data.description::text,
      inserted_event_data.time_start::timestamptz,
      inserted_event_data.time_end::timestamptz?,
      inserted_event_data.location::text,
      inserted_event_data.location_google_maps_link::text?,
      events.created_at::timestamptz,
      inserted_event_data.created_at::timestamptz
    from inserted_event_data
    join events
      on events.id = inserted_event_data.id
  |]
  where
    f (eventId, CreateEventInput{title, description, startTime, endTime, location, googleMapsLink, Types.CreateEventInput.password}) =
      (eventId, title, description, startTime, endTime, location, googleMapsLink)


sendEmailUpdate event = do
  conn <- asks connection
  eAttendees <- liftIO $ Hasql.run (Hasql.statement (Event.id event) statement) conn

  case eAttendees of
    Left err -> do
      liftIO $ putStrLn [i|Somethin went wrong when sending emails on event update: #{err}|]
      throwError err500 { errBody = "Something went wrong" }
    Right attendees -> do
      smtpConf <- asks smtpConfig
      liftIO $ forM_ attendees $ \attendee' -> do
        forkIO $ sendEventUpdateEmail smtpConf event attendee'
  where
    statement = fmap to <$>
      [vectorStatement|
        select
          attendees.event_id::uuid,
          attendees.email::text,
          attendee_data.name::text,
          attendee_data.status::text,
          attendee_data.plus_one::bool,
          attendee_data.rsvp_at::timestamptz
        from attendees
        join attendee_data
          on attendee_data.attendee_id = attendees.id
          and attendee_data.superseded_at is null
        where
          attendees.event_id = $1::uuid
          and attendee_data.status in ('coming', 'maybe_coming')
      |]
