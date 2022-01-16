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

import qualified Endpoints.AddVisit
import qualified Endpoints.CreateEvent
import qualified Endpoints.GetEvent
import           Types.CreateEventInput      (CreateEventInput)
import           Types.Event                 (Event)
import           Types.Visit                 (Visit)
import           Types.VisitPut              (VisitPut)

localPG :: Settings
localPG = settings "localhost" 5433 "postgres" "postgres" "events"

type API = EventsAPI :<|> CreateEventAPI :<|> VisitsAPI
type CreateEventAPI = "api" :> "v1" :> "events" :> ReqBody '[JSON] CreateEventInput :> Post '[JSON] Event
type EventsAPI = "api" :> "v1" :> "events" :> Capture "event_id" UUID :> Get '[JSON] Event
type VisitsAPI = "api" :> "v1" :> "visits" :> ReqBody '[JSON] VisitPut :> Put '[JSON] Visit

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
    :<|> Endpoints.AddVisit.addVisit connection
