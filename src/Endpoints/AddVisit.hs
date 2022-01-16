module Endpoints.AddVisit (addVisit) where

import           Control.Monad.IO.Class
import           Data.Profunctor        (dimap)
import           Hasql.Connection       (Connection)
import qualified Hasql.Session          as Hasql
import           Hasql.Statement        (Statement)
import           Hasql.TH               (maybeStatement, resultlessStatement,
                                         singletonStatement)

import           Data.Text              (pack)
import           Types.Visit            (Visit)
import qualified Types.Visit            as Visit
import           Types.VisitPut         (VisitPut (..))
import qualified Types.VisitPut         as VP


addVisit :: MonadIO m => Connection -> VisitPut -> m Visit
addVisit connection VisitPut{eventId, visitorId, status, plusOne} = do
  let session = do
                let visitTuple = (eventId, fromIntegral visitorId, pack (show status), plusOne)

                mExistingVisit <- Hasql.statement visitTuple [maybeStatement|
                    select
                      event_id::uuid,
                      visitor_id::bigint,
                      status::text,
                      plus_one::bool,
                      rsvp_at::timestamptz
                    from visits
                    where
                      event_id = $1::uuid
                      and visitor_id = $2::bigint
                      and status = lower($3::text)::visit_status
                      and plus_one = $4::bool
                      and superseded_at is null
                  |]

                case mExistingVisit of
                  Just existingVisit -> pure $ Visit.fromTuple existingVisit
                  Nothing -> do
                    Hasql.statement (eventId, fromIntegral visitorId) [resultlessStatement|
                        update visits
                        set superseded_at = now()
                        where
                          superseded_at is null
                          and event_id = $1::uuid
                          and visitor_id = $2::bigint
                      |]

                    Visit.fromTuple <$> Hasql.statement visitTuple [singletonStatement|
                        insert into visits (event_id, visitor_id, status, plus_one)
                        values ($1::uuid, $2::bigint, lower($3::text)::visit_status, $4::bool)
                        returning event_id::uuid, visitor_id::bigint, status::text, plus_one::bool, rsvp_at::timestamptz
                      |]

  eVisit <- liftIO $ Hasql.run session connection
  case eVisit of
    Right visit -> pure visit
    Left err -> do
      liftIO $ print err
      undefined -- TODO
