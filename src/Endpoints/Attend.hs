module Endpoints.Attend (attend) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Profunctor        (dimap, lmap)
import           Hasql.Connection       (Connection)
import qualified Hasql.Session          as Hasql
import           Hasql.Statement        (Statement)
import           Hasql.TH               (maybeStatement, resultlessStatement,
                                         singletonStatement)

import           Data.Text              (pack)
import           Data.Types.Injective   (to)
import           Data.UUID              (UUID)
import           Functions.GetEvent     (getEvent)
import           Types.AttendInput      (AttendInput (..))
import qualified Types.AttendInput      as VP
import           Types.Attendee         (Attendee, writeStatus)
import qualified Types.Attendee         as Attendee
import           Types.Event            (Event)


attend :: MonadIO m => Connection -> UUID -> AttendInput -> m Event
attend connection eventId attendee' = do
  let attendee = attendee' { eventId = eventId }
  let session = do
                mExistingAttendee <- Hasql.statement attendee findExistingStatement
                case mExistingAttendee of
                  Just existingAttendee -> pure existingAttendee
                  Nothing -> do
                    Hasql.statement attendee obsoleteOldAttendeeStatement
                    Hasql.statement attendee insertAttendeeStatement

  eAttendee <- liftIO $ Hasql.run session connection
  case eAttendee of
    Right attendee -> getEvent connection eventId
    Left err -> do
      liftIO $ print err
      undefined -- TODO

findExistingStatement :: Statement AttendInput (Maybe Attendee)
findExistingStatement = dimap to (fmap to) [maybeStatement|
                    select
                      event_id::uuid,
                      email::text,
                      first_name::text,
                      last_name::text,
                      status::text,
                      plus_one::bool,
                      rsvp_at::timestamptz
                    from attendees
                    where
                      event_id = $1::uuid
                      and email = $2::text
                      and status = $3::text::attendee_status
                      and plus_one = $4::bool
                      and superseded_at is null
                  |]
  where
    toTuple AttendInput{eventId, email, status, plusOne} = (eventId, email, writeStatus status, plusOne)

obsoleteOldAttendeeStatement :: Statement AttendInput ()
obsoleteOldAttendeeStatement = lmap to [resultlessStatement|
                        update attendees
                        set superseded_at = now()
                        where
                          superseded_at is null
                          and event_id = $1::uuid
                          and email = $2::text
                      |]

insertAttendeeStatement :: Statement AttendInput Attendee
insertAttendeeStatement = dimap to to [singletonStatement|
                        insert into attendees (event_id, email, first_name, last_name, status, plus_one)
                        values ($1::uuid, $2::text, $3::text, $4::text, lower($5::text)::attendee_status, $6::bool)
                        returning event_id::uuid, email::text, first_name::text, last_name::text, status::text, plus_one::bool, rsvp_at::timestamptz
                      |]