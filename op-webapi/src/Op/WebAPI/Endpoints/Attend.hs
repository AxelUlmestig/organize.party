{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.Attend (attend) where

import           Control.Monad                (when)
import           Control.Monad.Except         (MonadError (throwError))
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Reader         (MonadReader, asks)
import           Data.Profunctor              (lmap)
import qualified Data.Text                    as Text
import           Data.UUID                    (UUID)
{-
import           Hasql.Session                (CommandError (ResultError),
                                               ResultError (ServerError),
                                               SessionError (QueryError))
-}
import qualified Hasql.Session                as Hasql
import           Hasql.Statement              (Statement)
import           RIO                          (Text)
import           Servant                      (ServerError (errBody), err400,
                                               err404, err500)

import qualified Op.Db                        as Db
import           Op.WebAPI.Endpoints.GetEvent (getEvent)
import           Op.WebAPI.Types.AppEnv       (AppEnv (..))
import qualified Op.WebAPI.Types.Attendee     as Attendee
import           Op.WebAPI.Types.AttendInput  (AttendInput (..))
import qualified Op.WebAPI.Types.AttendInput  as VP
import           Op.WebAPI.Types.Event        (Event)


attend :: (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) => UUID -> AttendInput -> m Event
attend eventId attendee' = do
  let attendee = attendee' { VP.email = Text.toLower attendee'.email }

  when (eventId /= attendee.eventId) $
    throwError err400 { errBody = "Event id in the URL has to be the same as the event id in the body" }

  do
    hostUrl' <- asks (Text.pack . hostUrl)
    Db.queryDbOr handleErr do
      Hasql.statement (hostUrl', attendee) insertAttendeeStatement

  getEvent eventId
  where
    handleErr err = do
      liftIO $ print err
      case err of
        -- TODO: Look up the error types in the Hasql API
        -- QueryError _ _ (ResultError (ServerError "23503" _ _ _ _)) -> throwError err404 { errBody = "Event not found" }
        _                                                        -> throwError err500 { errBody = "Something went wrong" }


insertAttendeeStatement :: Statement (Text, AttendInput) ()
insertAttendeeStatement =
  lmap to'
    [Db.resultlessStatement|
      select add_attendee_data(
        host_url_ => $1::text,
        event_id_ => $2::uuid,
        email_ => $3::text,
        name_ => $4::text,
        status_ => $5::text::attendee_status,
        plus_one_ => $6::bool,
        get_notified_on_comments_ => $7::bool
      )::text
    |]
    where
      to' (hostUrl, AttendInput{..}) =
        (hostUrl, eventId, email, name, Attendee.writeStatus status, plusOne, getNotifiedOnComments)

