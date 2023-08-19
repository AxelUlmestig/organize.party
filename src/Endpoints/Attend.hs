module Endpoints.Attend (attend) where

import           Control.Concurrent     (forkIO)
import           Control.Monad          (void, when)
import           Control.Monad.Except   (MonadError (throwError))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Profunctor        (dimap, lmap)
import           Data.Text              (pack)
import           Data.Types.Injective   (to)
import           Data.UUID              (UUID)
import           Email                  (sendEmailInvitation)
import           Endpoints.GetEvent     (getEvent)
import           Hasql.Connection       (Connection)
import           Hasql.Session          (CommandError (ResultError),
                                         QueryError (QueryError),
                                         ResultError (ServerError))
import qualified Hasql.Session          as Hasql
import           Hasql.Statement        (Statement)
import           Hasql.TH               (maybeStatement, resultlessStatement,
                                         singletonStatement)
import           Servant                (ServerError (errBody), err400, err404,
                                         err500)

import qualified Email
import           Types.AppEnv           (AppEnv (..), SmtpConfig (..))
import qualified Types.Attendee         as Attendee
import           Types.Attendee         (Attendee, writeStatus)
import qualified Types.AttendInput      as VP
import           Types.AttendInput      (AttendInput (..))
import           Types.Event            (Event)


attend :: (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) => UUID -> AttendInput -> m Event
attend eventId attendee@AttendInput { eventId = bodyEventId } = do
  when (eventId /= bodyEventId) $
    throwError err400 { errBody = "Event id in the URL has to be the same as the event id in the body" }

  let session = do
        emailSentAlready <- Hasql.statement attendee emailSentAlreadyStatement
        mExistingAttendee <- Hasql.statement attendee findExistingStatement

        insertedAttendee@Attendee.Attendee{Attendee.status} <- case mExistingAttendee of
            Just existingAttendee -> pure existingAttendee
            Nothing -> do
              Hasql.statement attendee obsoleteOldAttendeeStatement
              Hasql.statement attendee insertAttendeeStatement

        pure (insertedAttendee, not emailSentAlready && status /= Attendee.NotComing)


  conn <- asks connection
  eAttendee <- liftIO $ Hasql.run session conn
  case eAttendee of
    Right (attendee, shouldSendEmail) -> do
      event <- getEvent eventId

      when shouldSendEmail $ do
        smtpConf <- asks smtpConfig
        void . liftIO . forkIO $ sendEmailInvitation smtpConf event attendee
      pure event
    Left err -> do
      liftIO $ print err
      case err of
        QueryError _ _ (ResultError (ServerError "23503" _ _ _ _)) -> throwError err404 { errBody = "Event not found" }
        _                                                        -> throwError err500 { errBody = "Something went wrong" }

findExistingStatement :: Statement AttendInput (Maybe Attendee)
findExistingStatement = dimap to (fmap to) [maybeStatement|
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
                      and email = $2::text
                      and name = $3::text
                      and status = $4::text::attendee_status
                      and plus_one = $5::bool
                      and superseded_at is null
                  |]
  where
    toTuple AttendInput{eventId, email, status, plusOne} = (eventId, email, writeStatus status, plusOne)

obsoleteOldAttendeeStatement :: Statement AttendInput ()
obsoleteOldAttendeeStatement =
  lmap to
    [resultlessStatement|
      update attendees
      set superseded_at = now()
      where
        superseded_at is null
        and event_id = $1::uuid
        and email = $2::text
    |]

insertAttendeeStatement :: Statement AttendInput Attendee
insertAttendeeStatement =
  dimap to to
    [singletonStatement|
      insert into attendees (event_id, email, name, status, plus_one)
      values ($1::uuid, $2::text, $3::text, lower($4::text)::attendee_status, $5::bool)
      returning event_id::uuid, email::text, name::text, status::text, plus_one::bool, rsvp_at::timestamptz
    |]

emailSentAlreadyStatement :: Statement AttendInput Bool
emailSentAlreadyStatement = lmap (\AttendInput{eventId, email} -> (eventId, email))
  [singletonStatement|
    select exists (
      select 1
      from attendees
      where
        event_id = $1::uuid
        and email = $2::text
        and status in ('coming', 'maybe_coming')
    )::bool
  |]
