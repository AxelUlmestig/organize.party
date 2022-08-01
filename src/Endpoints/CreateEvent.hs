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
    insert into events (title, description, time_start, time_end, location, location_google_maps_link, password_salt, password_hash)
    select $1::text, $2::text, $3::timestamptz, $4::timestamptz?, $5::text, $6::text?, salt, digest($7::text || salt, 'sha256')
    from (
      select md5(random()::text || clock_timestamp()::text) as salt
    ) t
    returning
      id::uuid,
      title::text,
      description::text,
      time_start::timestamptz,
      time_end::timestamptz?,
      location::text,
      location_google_maps_link::text?
  |]
