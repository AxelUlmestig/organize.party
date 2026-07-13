{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.CreateEvent (createEvent) where

import           Control.Monad.Except             (MonadError (..))
import qualified Data.Aeson                       as Aeson
import           Data.Coerce                      (coerce)
import           Data.String.Interpolate          (i)
import           RIO                              hiding (to)
import           Servant                          (ServerError (..), err500)

import qualified Op.Db                            as Db
import           Op.WebAPI.Types.CreateEventInput (CreateEventInput (..))
import           Op.WebAPI.Types.Event            (Event (..))
import           Op.WebAPI.Types.Password         (Password (..))


createEvent ::
  ( MonadError ServerError m
  , MonadIO m
  , MonadReader env m
  , Db.HasDbConnection env
  , HasLogFunc env
  )
  => CreateEventInput
  -> m Event
createEvent input = do
  json <- do
    Db.queryDbOr Db.printAndThrow500 do
      let CreateEventInput{..} = input
      Db.statement
        (title, description, startTime, endTime, location, photoId, coerce password)
        [Db.singletonStatement|
          select create_event(
            title_        => $1::text,
            description_  => $2::text,
            time_start_   => $3::timestamptz,
            time_end_     => $4::timestamptz?,
            location_     => $5::text,
            photo_id_     => $6::uuid?,
            password_     => $7::text
          )::jsonb
        |]

  case Aeson.fromJSON json of
    Aeson.Success event -> pure event
    Aeson.Error err -> do
      logError [i|Could not parse event json after event creation: #{Aeson.encode json}\n\nerr: #{err}|]
      throwError err500 { errBody = "Something went wrong" }

