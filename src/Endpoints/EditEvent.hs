module Endpoints.EditEvent (editEvent) where

import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Profunctor        (dimap)
import           Data.Types.Isomorphic  (to)
import           Data.UUID              (UUID)
import qualified Hasql.Session          as Hasql
import           Hasql.Statement        (Statement)
import           Hasql.TH               (maybeStatement, singletonStatement)
import           Servant                (ServerError (..), err403, err404,
                                         err500)

import           Types.AppEnv           (AppEnv, connection)
import           Types.CreateEventInput
import           Types.Event            (Event)

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
    Right (Success event) -> pure event

session :: (UUID, CreateEventInput) -> Hasql.Session EditResult
session (eventId, input) = do
  exists <- Hasql.statement eventId existsStatement
  if not exists
  then pure NotFound
  else do
    maybeEvent <- Hasql.statement (eventId, input) updateIfPasswordMatchesStatement
    case maybeEvent of
      Nothing    -> pure Forbidden
      Just event -> pure $ Success event

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
    f (eventId, CreateEventInput{title, description, startTime, endTime, location, googleMapsLink, password}) =
      (eventId, title, description, startTime, endTime, location, googleMapsLink, password)
