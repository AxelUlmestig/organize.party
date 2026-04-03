{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.Attend (attend) where

import           Control.Monad.Except         (MonadError (throwError))
import           Data.Profunctor              (lmap)
import           Data.String.Interpolate      (i)
import qualified Data.Text                    as Text
import           Data.UUID                    (UUID)
import           RIO
import           Servant                      (ServerError (errBody), err400,
                                               err404, err500)

import qualified Op.Db                        as Db
import           Op.WebAPI.Endpoints.GetEvent (getEvent)
import qualified Op.WebAPI.Types.Attendee     as Attendee
import           Op.WebAPI.Types.AttendInput  (AttendInput (..))
import qualified Op.WebAPI.Types.AttendInput  as VP
import           Op.WebAPI.Types.Event        (Event)
import           Op.WebAPI.Types.HasHostUrl   (HasHostUrl (..))


attend ::
  ( MonadError ServerError m
  , MonadIO m
  , MonadReader env m
  , HasHostUrl env
  , Db.HasDbConnection env
  , HasLogFunc env
  )
  => UUID
  -> AttendInput
  -> m Event
attend eventId attendee' = do
  let attendee = attendee' { VP.email = Text.toLower attendee'.email }

  when (eventId /= attendee.eventId) $
    throwError err400 { errBody = "Event id in the URL has to be the same as the event id in the body" }

  do
    hostUrl <- asks getHostUrl
    Db.queryDbOr handleErr do
      Db.statement (hostUrl, attendee) insertAttendeeStatement

  getEvent eventId
  where
    handleErr err = do
      case err of
        Db.StatementSessionError _ _ _ _ _ (Db.ServerStatementError (Db.ServerError "23503" _ _ _ _)) ->
          throwError err404 { errBody = "Event not found" }
        _ -> do
          logError [i|Something went wrong when attending event: #{err}|]
          throwError err500 { errBody = "Something went wrong" }


insertAttendeeStatement :: Db.Statement (Text, AttendInput) ()
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

