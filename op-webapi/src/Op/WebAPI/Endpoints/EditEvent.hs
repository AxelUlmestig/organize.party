{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes         #-}

module Op.WebAPI.Endpoints.EditEvent (editEvent) where

import           Control.Monad.Except             (MonadError (..))
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Reader             (MonadReader, asks)
import qualified Data.Text                        as Text
import           Data.UUID                        (UUID)
import           RIO                              (liftIO)
import           Servant                          (ServerError (..), err403,
                                                   err404, err500)

import qualified Op.Db                            as Db
import           Op.WebAPI.Endpoints.GetEvent     (getEvent)
import           Op.WebAPI.Types.AppEnv           (AppEnv (..))
import           Op.WebAPI.Types.CreateEventInput
import           Op.WebAPI.Types.Event            (Event)

editEvent :: (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) => UUID -> CreateEventInput -> m Event
editEvent eventId CreateEventInput{..} = do
  emailHostUrl <- asks (Text.pack . hostUrl)
  Db.queryDbOr handleError do
    Db.statement
      (emailHostUrl, eventId, title, description, startTime, endTime, location, googleMapsLink, password)
      [Db.resultlessStatement|
        select edit_event(
          host_url_ => $1::text,
          event_id_ => $2::uuid,
          title_ => $3::text,
          description_ => $4::text,
          start_time_ => $5::timestamptz,
          end_time_ => $6::timestamptz?,
          location_ => $7::text,
          google_maps_link_ => $8::text?,
          password_ => $9::text
        )::text
      |]

  getEvent eventId
  where
    handleError err =
      case err of
        Db.StatementSessionError _ _ _ _ _ (Db.ServerStatementError (Db.ServerError errorCode _ _ _ _)) -> do
          case errorCode of
            "P0403" -> throwError err403 { errBody = "Password didn't match" }
            "P0404" -> throwError err404 { errBody = "Event not found" }
            _ -> do
              liftIO $ print err
              throwError err500 { errBody = "Something went wrong" }
        _ -> do
          liftIO $ print err
          throwError err500 { errBody = "Something went wrong" }

