module Endpoints.GetEvent (getEvent, getAttendees, getAttendeesStatement, getCommentsStatement) where

import           Control.Monad.Except   (MonadError (throwError))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Types.Injective   (to)
import           Data.UUID              (UUID)
import           Hasql.Connection       (Connection)
import           Hasql.Session          (CommandError (ResultError),
                                         QueryError (QueryError),
                                         ResultError (UnexpectedAmountOfRows))
import qualified Hasql.Session          as Hasql
import           Hasql.Statement        (Statement)
import           Hasql.TH               (maybeStatement, vectorStatement)
import           Servant                (ServerError (..), err404, err500)

import qualified Data.Vector            as Vector
import           Types.AppEnv
import           Types.Event            (Attendee, Comment (..), Event (..))

getEvent :: (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) => UUID -> m Event
getEvent eventId = do
    let statement = do
            mEvent <- Hasql.statement eventId getEventStatement
            case mEvent of
              Nothing    -> pure Nothing
              Just event -> do
                attendees <- Hasql.statement event.id getAttendeesStatement
                comments <- Hasql.statement event.id getCommentsStatement
                pure $ Just event { attendees, comments }

    conn <- asks connection
    eEvent <- liftIO $ Hasql.run statement conn
    case eEvent of
      Right (Just event)  -> getAttendees event
      Right Nothing       -> throwError err404 { errBody = "Event not found" }
      Left err            -> do
        liftIO $ print err
        throwError err500 { errBody = "Something went wrong" }


getEventStatement :: Statement UUID (Maybe Event)
getEventStatement =
  fmap to <$>
    [maybeStatement|
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
getAttendees event@Event{Types.Event.id} = do
  conn <- asks connection
  eAttendees <- liftIO $ Hasql.run (Hasql.statement id getAttendeesStatement) conn
  case eAttendees of
    Left err -> do
      liftIO $ print err
      throwError err500 { errBody = "Something went wrong" }
    Right attendees -> return $ event { attendees = attendees }

getAttendeesStatement :: Statement UUID [Attendee]
getAttendeesStatement =
  fmap to . Vector.toList <$>
    [vectorStatement|
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

getCommentsStatement :: Statement UUID [Comment]
getCommentsStatement =
  fmap to . Vector.toList <$>
    [vectorStatement|
      select
        name::text,
        comment::text,
        created_at::timestamptz
      from comments
      where
        event_id = $1::uuid
    |]
