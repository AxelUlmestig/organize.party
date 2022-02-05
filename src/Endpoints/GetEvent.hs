module Endpoints.GetEvent (getEvent, getAttendees) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Types.Injective   (to)
import           Data.UUID              (UUID)
import           Hasql.Connection       (Connection)
import           Hasql.Session          (CommandError (ResultError),
                                         QueryError (QueryError),
                                         ResultError (UnexpectedAmountOfRows))
import qualified Hasql.Session          as Hasql
import           Hasql.Statement        (Statement)
import           Hasql.TH               (singletonStatement, vectorStatement)
import           Servant                (ServerError (..), err404, err500)

import           Control.Monad.Except   (MonadError (throwError))
import           Types.Event            (Attendee, Event (..))

getEvent :: (MonadError ServerError m, MonadIO m) => Connection -> UUID -> m Event
getEvent connection eventId = do
    eEvent <- liftIO $ Hasql.run (Hasql.statement eventId statement) connection
    case eEvent of
      Right event -> getAttendees connection event
      Left err    -> do
        liftIO $ print err
        case err of
          QueryError _ _ (ResultError (UnexpectedAmountOfRows _)) -> throwError err404 { errBody = "Event not found" }
          _                                                       -> throwError err500 { errBody = "Something went wrong" }


statement :: Statement UUID Event
statement = to <$> [singletonStatement|
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

getAttendees :: (MonadError ServerError m, MonadIO m) => Connection -> Event -> m Event
getAttendees connection event@Event{Types.Event.id} = do
  eAttendees <- liftIO $ Hasql.run (Hasql.statement id getAttendeesStatement) connection
  case eAttendees of
    Left err -> do
      liftIO $ print err
      throwError err500 { errBody = "Something went wrong" }
    Right attendees -> return $ event { attendees = attendees }

getAttendeesStatement :: Statement UUID [Attendee]
getAttendeesStatement = fmap to . vectorToList <$> [vectorStatement|
                                                     select
                                                       name::text,
                                                       status::text,
                                                       plus_one::bool
                                                     from attendees
                                                     where
                                                       event_id = $1::uuid
                                                       and superseded_at is null
                                                     order by
                                                       status,
                                                       rsvp_at desc
                                                   |]
  where
    vectorToList = foldr (:) []
