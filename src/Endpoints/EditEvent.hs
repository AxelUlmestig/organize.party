module Endpoints.EditEvent (editEvent) where

import           Control.Concurrent     (forkIO)
import           Control.Monad          (forM_)
import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Profunctor        (dimap)
import           Data.Types.Isomorphic  (to)
import           Data.UUID              (UUID)
import qualified Hasql.Session          as Hasql
import           Hasql.Statement        (Statement)
import           Hasql.TH               (maybeStatement, singletonStatement,
                                         vectorStatement)
import           Servant                (ServerError (..), err403, err404,
                                         err500)

import           Email                  (sendEmailInvitation)
import           Endpoints.GetEvent     (getAttendeesStatement)
import           Types.AppEnv           (AppEnv (..), SmtpConfig (..),
                                         connection)
import           Types.CreateEventInput
import           Types.Event            (Event)
import qualified Types.Event            as Event

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
  exists <- Hasql.statement eventId existsStatement
  if not exists
  then pure NotFound
  else do
    maybeEvent <- Hasql.statement (eventId, input) updateIfPasswordMatchesStatement
    case maybeEvent of
      Nothing    -> pure Forbidden
      Just event -> do
        attendees <- Hasql.statement (Event.id event) getAttendeesStatement
        pure $ Success $ event { Event.attendees = attendees }

existsStatement :: Statement UUID Bool
existsStatement =
  [singletonStatement|
    select exists(select 1 from events where id = $1::uuid)::bool
  |]

updateIfPasswordMatchesStatement :: Statement (UUID, CreateEventInput) (Maybe Event)
updateIfPasswordMatchesStatement =
  dimap f (fmap to)
  [maybeStatement|
    update events
    set
      title = $2::text,
      description = $3::text,
      time_start = $4::timestamptz,
      time_end = $5::timestamptz?,
      location = $6::text,
      location_google_maps_link = $7::text?
    where
      id = $1::uuid
      and password_hash = digest($8::text|| password_salt, 'sha256')::text
    returning
      id::uuid,
      title::text,
      description::text,
      time_start::timestamptz,
      time_end::timestamptz?,
      location::text,
      location_google_maps_link::text?
  |]
  where
    f (eventId, CreateEventInput{title, description, startTime, endTime, location, googleMapsLink, Types.CreateEventInput.password}) =
      (eventId, title, description, startTime, endTime, location, googleMapsLink, password)

sendEmailUpdate event = do
  conn <- asks connection
  eAttendees <- liftIO $ Hasql.run (Hasql.statement (Event.id event) statement) conn

  case eAttendees of
    Left err -> do
      liftIO $ print err
      throwError err500 { errBody = "Something went wrong" }
    Right attendees -> do
      smtpConf <- asks smtpConfig
      liftIO $ forM_ attendees $ \attendee' -> do
        forkIO $ sendEmailInvitation smtpConf event attendee'
  where
    statement = fmap to <$>
      [vectorStatement|
        select
          event_id::uuid,
          email::text,
          name::text,
          status::text,
          plus_one::bool,
          rsvp_at::timestamptz
        from attendees
        where
          event_id = $1::uuid
          and superseded_at is null
          and status in ('coming', 'maybe_coming')
      |]
