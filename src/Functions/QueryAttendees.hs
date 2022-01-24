module Functions.QueryAttendees (queryAttendees) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Types.Injective   (to)
import           Data.UUID              (UUID)
import           Hasql.Connection       (Connection)
import qualified Hasql.Session          as Hasql
import           Hasql.Statement        (Statement)
import           Hasql.TH               (vectorStatement)
import           Types.Event            (Attendee, Event (..))

queryAttendees :: MonadIO m => Connection -> Event -> m Event
queryAttendees connection event@Event{Types.Event.id} = do
  eAttendees <- liftIO $ Hasql.run (Hasql.statement id getAttendeesStatement) connection
  case eAttendees of
    Left err -> do
      liftIO $ print err
      undefined
    Right attendees -> return $ event { attendees = attendees }

getAttendeesStatement :: Statement UUID [Attendee]
getAttendeesStatement = fmap to . vectorToList <$> [vectorStatement|
                                                     select
                                                       first_name::text,
                                                       last_name::text,
                                                       status::text,
                                                       plus_one::bool
                                                     from attendees
                                                     where
                                                      event_id = $1::uuid
                                                      and superseded_at is null
                                                   |]
  where
    vectorToList = foldr (:) []
