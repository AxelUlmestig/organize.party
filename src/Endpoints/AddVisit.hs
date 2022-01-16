module Endpoints.AddVisit (addVisit) where

import           Control.Monad.IO.Class
import           Data.Profunctor        (dimap, lmap)
import           Hasql.Connection       (Connection)
import qualified Hasql.Session          as Hasql
import           Hasql.Statement        (Statement)
import           Hasql.TH               (maybeStatement, resultlessStatement,
                                         singletonStatement)

import           Data.Text              (pack)
import           Types.Visit            (Visit, writeStatus)
import qualified Types.Visit            as Visit
import           Types.VisitPut         (VisitPut (..))
import qualified Types.VisitPut         as VP


addVisit :: MonadIO m => Connection -> VisitPut -> m Visit
addVisit connection visit = do
  let session = do
                mExistingVisit <- Hasql.statement visit findExistingStatement
                case mExistingVisit of
                  Just existingVisit -> pure existingVisit
                  Nothing -> do
                    Hasql.statement visit obsoleteOldVisitStatement
                    Hasql.statement visit insertVisitStatement

  eVisit <- liftIO $ Hasql.run session connection
  case eVisit of
    Right visit -> pure visit
    Left err -> do
      liftIO $ print err
      undefined -- TODO

findExistingStatement :: Statement VisitPut (Maybe Visit)
findExistingStatement = dimap toTuple (fmap Visit.fromTuple) [maybeStatement|
                    select
                      event_id::uuid,
                      email::text,
                      first_name::text,
                      last_name::text,
                      status::text,
                      plus_one::bool,
                      rsvp_at::timestamptz
                    from visits
                    where
                      event_id = $1::uuid
                      and email = $2::text
                      and status = $3::text::visit_status
                      and plus_one = $4::bool
                      and superseded_at is null
                  |]
  where
    toTuple VisitPut{eventId, email, status, plusOne} = (eventId, email, writeStatus status, plusOne)

obsoleteOldVisitStatement :: Statement VisitPut ()
obsoleteOldVisitStatement = lmap toTuple [resultlessStatement|
                        update visits
                        set superseded_at = now()
                        where
                          superseded_at is null
                          and event_id = $1::uuid
                          and email = $2::text
                      |]
  where
    toTuple VP.VisitPut{eventId, email} = (eventId, email)

insertVisitStatement :: Statement VisitPut Visit
insertVisitStatement = dimap VP.toTuple Visit.fromTuple [singletonStatement|
                        insert into visits (event_id, email, first_name, last_name, status, plus_one)
                        values ($1::uuid, $2::text, $3::text, $4::text, lower($5::text)::visit_status, $6::bool)
                        returning event_id::uuid, email::text, first_name::text, last_name::text, status::text, plus_one::bool, rsvp_at::timestamptz
                      |]
