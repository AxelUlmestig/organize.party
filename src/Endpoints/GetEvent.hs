module Endpoints.GetEvent (getEvent) where

import           Control.Monad.IO.Class
import           Data.UUID              (UUID)
import qualified Hasql.Session          as Hasql
import           Hasql.Statement        (Statement)
import           Hasql.TH               (singletonStatement)

import           Hasql.Connection       (Connection)
import           Types.Event            (Event)
import qualified Types.Event            as E

getEvent :: MonadIO m => Connection -> UUID -> m Event
getEvent connection eventId = do
    eEvent <- liftIO $ Hasql.run (Hasql.statement eventId statement) connection
    case eEvent of
      Left err    -> do
        liftIO $ print err
        undefined -- TODO
      Right event -> pure event

statement :: Statement UUID Event
statement = E.fromTuple <$> [singletonStatement|
    select
       id::uuid,
       title::text,
       description::text,
       time_start::timestamptz,
       time_end::timestamptz,
       location::text,
       location_google_maps_link::text?
    from events
    where id = $1::uuid
  |]
