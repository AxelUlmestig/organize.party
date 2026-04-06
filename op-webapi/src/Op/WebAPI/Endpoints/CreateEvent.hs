{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.CreateEvent (createEvent) where

import           Control.Monad.Except             (MonadError (..))
import           Data.Coerce                      (coerce)
import           RIO                              hiding (to)
import           Servant                          (ServerError (..))

import qualified Op.Db                            as Db
import           Op.WebAPI.Types.CreateEventInput (CreateEventInput (..))
import           Op.WebAPI.Types.Event            (Event (..))
import           Op.WebAPI.Types.Password         (Password (..))


createEvent ::
  ( MonadError ServerError m
  , MonadIO m
  , MonadReader env m
  , Db.HasDbConnection env
  )
  => CreateEventInput
  -> m Event
createEvent input = do
  let tupleToEvent (id, title, description, startTime, endTime, location, googleMapsLink, createdAt, modifiedAt) = Event{attendees = [], comments = [], ..}

  fmap tupleToEvent do
    Db.queryDbOr Db.printAndThrow500 do
      let CreateEventInput{..} = input
      Db.statement
        (title, description, startTime, endTime, location, googleMapsLink, coerce password)
        [Db.singletonStatement|
          with
            event as (
              insert into events (password_salt, password_hash)
                select salt, digest($7::text || salt, 'sha256')
                from (
                  select md5(random()::text || clock_timestamp()::text) as salt
                ) t
              returning *
            ),

            inserted_event_data as (
              insert into event_data (id, title, description, time_start, time_end, location, location_google_maps_link)
              select event.id, $1::text, $2::text, $3::timestamptz, $4::timestamptz?, $5::text, $6::text?
              from event
              returning *
            )

            select
              inserted_event_data.id::uuid,
              inserted_event_data.title::text,
              inserted_event_data.description::text,
              inserted_event_data.time_start::timestamptz,
              inserted_event_data.time_end::timestamptz?,
              inserted_event_data.location::text,
              inserted_event_data.location_google_maps_link::text?,
              event.created_at::timestamptz,
              inserted_event_data.created_at::timestamptz
            from inserted_event_data
            cross join event
          |]

