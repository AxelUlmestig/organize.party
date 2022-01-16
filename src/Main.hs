{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeOperators  #-}


module Main where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Int                    (Int64)
import           Data.Text                   (Text, pack)
import           Data.UUID                   (UUID)
import           Hasql.Connection            (Connection, Settings, acquire,
                                              settings)
import qualified Hasql.Session               as Hasql
import           Hasql.Statement             (Statement)
import           Hasql.TH                    (maybeStatement,
                                              resultlessStatement,
                                              singletonStatement)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant
import           Servant.API
import           Text.Read                   (readMaybe)

import qualified Endpoints.CreateEvent
import qualified Endpoints.GetEvent
import           Types.CreateEventInput      (CreateEventInput)
import qualified Types.CreateEventInput      as CE
import           Types.Event                 (Event (Event))
import qualified Types.Event                 as E
import           Types.Visit                 (Visit)
import qualified Types.Visit                 as Visit
import           Types.VisitPut              (VisitPut (..))
import qualified Types.VisitPut              as VP
import           Types.Visitor               (Visitor)
import qualified Types.Visitor               as Visitor

localPG :: Settings
localPG = settings "localhost" 5433 "postgres" "postgres" "events"

type API = EventsAPI :<|> CreateEventAPI :<|> VisitsAPI
type CreateEventAPI = "api" :> "v1" :> "events" :> ReqBody '[JSON] CreateEventInput :> Post '[JSON] Event
type EventsAPI = "api" :> "v1" :> "events" :> Capture "event_id" UUID :> Get '[JSON] Event
type VisitsAPI = "api" :> "v1" :> "visits" :> ReqBody '[JSON] VP.VisitPut :> Put '[JSON] Visit

api :: Proxy API
api = Proxy

app :: Connection -> Application
app = simpleCors . serve api . server

main :: IO ()
main = do
  eConnection <- acquire localPG
  case eConnection of
    Left err -> print err
    Right connection -> do
      putStrLn "listening on port 8081..."
      run 8081 $ app connection

server :: Connection -> Server API
server connection = Endpoints.GetEvent.getEvent connection
    :<|> Endpoints.CreateEvent.createEvent connection
    :<|> addVisit

  where
    addVisit :: VP.VisitPut -> Handler Visit
    addVisit VisitPut{eventId, visitorId, status, plusOne} = do
      let session = do
                    let visitTuple = (fromIntegral eventId, fromIntegral visitorId, pack (show status), plusOne)

                    mExistingVisit <- Hasql.statement visitTuple [maybeStatement|
                        select
                          event_id::bigint,
                          visitor_id::bigint,
                          status::text,
                          plus_one::bool,
                          rsvp_at::timestamptz
                        from visits
                        where
                          event_id = $1::bigint
                          and visitor_id = $2::bigint
                          and status = lower($3::text)::visit_status
                          and plus_one = $4::bool
                          and superseded_at is null
                      |]

                    case mExistingVisit of
                      Just existingVisit -> pure $ Visit.fromTuple existingVisit
                      Nothing -> do
                        Hasql.statement (fromIntegral eventId, fromIntegral visitorId) [resultlessStatement|
                            update visits
                            set superseded_at = now()
                            where
                              superseded_at is null
                              and event_id = $1::bigint
                              and visitor_id = $2::bigint
                          |]

                        Visit.fromTuple <$> Hasql.statement visitTuple [singletonStatement|
                            insert into visits (event_id, visitor_id, status, plus_one)
                            values ($1::bigint, $2::bigint, lower($3::text)::visit_status, $4::bool)
                            returning event_id::bigint, visitor_id::bigint, status::text, plus_one::bool, rsvp_at::timestamptz
                          |]

      eVisit <- liftIO $ Hasql.run session connection
      case eVisit of
        Right visit -> pure visit
        Left err -> do
          liftIO $ print err
          undefined -- TODO
