module Endpoints.Attend (attend) where

import           Control.Concurrent     (forkIO)
import           Control.Monad          (void, when)
import           Control.Monad.Except   (MonadError (throwError))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Profunctor        (dimap, lmap)
import qualified Data.Text              as Text
import           Data.Types.Injective   (to)
import           Data.UUID              (UUID)
import           Email                  (EmailData (..), sendEmailInvitation)
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
attend eventId attendee' = do
  let attendee = attendee' { VP.email = Text.toLower attendee'.email }

  when (eventId /= attendee.eventId) $
    throwError err400 { errBody = "Event id in the URL has to be the same as the event id in the body" }

  let session = do
        emailSentAlready <- Hasql.statement attendee emailSentAlreadyStatement
        mExistingAttendee <- Hasql.statement attendee findExistingStatement

        insertedAttendee@Attendee.Attendee{Attendee.status} <- case mExistingAttendee of
            Just existingAttendee -> pure existingAttendee
            Nothing -> Hasql.statement attendee insertAttendeeStatement

        pure (insertedAttendee, not emailSentAlready && status /= Attendee.NotComing)


  conn <- asks connection
  eAttendee <- liftIO $ Hasql.run session conn
  case eAttendee of
    Right (attendee, shouldSendEmail) -> do
      event <- getEvent eventId

      when shouldSendEmail $ do
        smtpConf <- asks smtpConfig
        emailHostUrl <- asks hostUrl
        void . liftIO . forkIO $
          let emailData = EmailData {email = attendee.email, recipientName = attendee.name, unsubscribeId = attendee.unsubscribeId, ..}
          in sendEmailInvitation emailData smtpConf event
      pure event
    Left err -> do
      liftIO $ print err
      case err of
        QueryError _ _ (ResultError (ServerError "23503" _ _ _ _)) -> throwError err404 { errBody = "Event not found" }
        _                                                        -> throwError err500 { errBody = "Something went wrong" }

findExistingStatement :: Statement AttendInput (Maybe Attendee)
findExistingStatement =
  dimap to (fmap to)
    [maybeStatement|
      select
        attendees.event_id::uuid,
        attendees.email::text,
        attendee_data.name::text,
        attendee_data.status::text,
        attendee_data.plus_one::bool,
        attendee_data.rsvp_at::timestamptz,
        attendees.unsubscribe_id::uuid
      from attendee_data
      join attendees
        on attendees.id = attendee_data.attendee_id
      where
        attendees.event_id = $1::uuid
        and attendees.email = $2::text
        and attendee_data.name = $3::text
        and attendee_data.status = $4::text::attendee_status
        and attendee_data.plus_one = $5::bool
        and attendee_data.get_notified_on_comments = $6::bool
        and attendee_data.superseded_at is null
    |]
  where
    toAttendee (unsubscribeId, eventId, email, name, status, plusOne, rsvpAt) = (unsubscribeId, to (eventId, email, name, status, plusOne, rsvpAt))
    toTuple AttendInput{eventId, email, status, plusOne} = (eventId, email, writeStatus status, plusOne)

insertAttendeeStatement :: Statement AttendInput Attendee
insertAttendeeStatement =
  dimap to to
    [singletonStatement|
      select
        event_id::uuid,
        email::text,
        name::text,
        status::text,
        plus_one::bool,
        rsvp_at::timestamptz,
        unsubscribe_id::uuid
      from add_attendee_data(
        event_id_ => $1::uuid,
        email_ => $2::text,
        name_ => $3::text,
        status_ => $4::text::attendee_status,
        plus_one_ => $5::bool,
        get_notified_on_comments_ => $6::bool
      ) as attendee_id
    |]

emailSentAlreadyStatement :: Statement AttendInput Bool
emailSentAlreadyStatement = lmap (\AttendInput{eventId, email} -> (eventId, email))
  [singletonStatement|
    select exists (
      select 1
      from attendee_data
      where
        status in ('coming', 'maybe_coming')
        and attendee_id in (
          select id
          from attendees
          where
            event_id = $1::uuid
            and email = $2::text
            and unsubscribed_at is null
        )
    )::bool
  |]
