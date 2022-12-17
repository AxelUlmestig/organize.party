module Endpoints.CreateEvent (createEvent) where

import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Profunctor        (dimap)
import           Data.Types.Isomorphic  (to)
import qualified Hasql.Session          as Hasql
import           Hasql.Statement        (Statement)
import           Hasql.TH               (singletonStatement)
import           Servant                (ServerError (..), err500)

import           Types.AppEnv
import           Types.CreateEventInput (CreateEventInput)
import qualified Types.CreateEventInput as CE
import           Types.Event            (Event)
import qualified Types.Event            as E


createEvent :: (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) => CreateEventInput -> m Event
createEvent input = do
  conn <- asks connection
  eEvent <- liftIO $ Hasql.run (Hasql.statement input statement) conn
  case eEvent of
    Left err    -> do
      liftIO $ print err
      throwError err500 { errBody = "Something went wrong" }
    Right event -> pure event

statement :: Statement CreateEventInput Event
statement = dimap to to
  [singletonStatement|
    with
      event as (
        insert into events (password_salt, password_hash)
          select salt, digest($7::text || salt, 'sha256')
          from (
            select md5(random()::text || clock_timestamp()::text) as salt
          ) t
        returning *
      ),

      inserted_event_data as (
        insert into event_data (id, title, description, time_start, time_end, location, location_google_maps_link)
        select event.id, $1::text, $2::text, $3::timestamptz, $4::timestamptz?, $5::text, $6::text?
        from event
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
        event.created_at::timestamptz,
        inserted_event_data.created_at::timestamptz
      from inserted_event_data
      cross join event
  |]
