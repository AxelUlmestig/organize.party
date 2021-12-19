{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.IO.Class      (liftIO)
import           Database.PostgreSQL.Simple  (ConnectInfo (..), Only (..),
                                              connect, defaultConnectInfo,
                                              execute, query, query_,
                                              withTransaction)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant
import           Servant.API

import           Types.Event                 (Event)
import qualified Types.Event                 as E
import           Types.Visit                 (Visit)
import qualified Types.VisitPut              as VP
import           Types.Visitor               (Visitor)

localPG :: ConnectInfo
localPG = defaultConnectInfo
        { connectHost = "localhost"
        , connectPort = 5433
        , connectDatabase = "events"
        , connectUser = "postgres"
        , connectPassword = "postgres"
        }

type API = EventsAPI :<|> VisitorsAPI :<|> VisitsAPI
type EventsAPI = "api" :> "v1" :> "events" :> Capture "event_id" String :> Get '[JSON] Event
type VisitorsAPI = "api" :> "v1" :> "visitors" :> QueryParam "email" String :> Get '[JSON] Visitor
type VisitsAPI = "api" :> "v1" :> "visits" :> ReqBody '[JSON] VP.VisitPut :> Put '[JSON] Visit

api :: Proxy API
api = Proxy

app :: Application
app = simpleCors $ serve api server

main :: IO ()
main = do
  putStrLn "listening on port 8081..."
  run 8081 app

server :: Server API
server = event
    :<|> visitors
    :<|> addVisit

  where
    event eventId = do
      conn <- liftIO $ connect localPG
      rows <- liftIO $ query conn "select * from events where id = ?" [eventId]
      case rows of
        (row:_) -> return row
        _       -> undefined -- TODO

    visitors :: Maybe String -> Handler Visitor
    visitors Nothing = throwError $ err400 { errBody = "supply an 'email' query param" }
    visitors (Just email) = do
      conn <- liftIO $ connect localPG
      rows <- liftIO $ query conn "select id, email::text, first_name, last_name from visitors where email = ?" [email]
      case rows of
        (visitor:_) -> return visitor
        _           -> throwError $ err404 { errBody = "visitor not found" }

    addVisit :: VP.VisitPut -> Handler Visit
    addVisit v = do
      conn <- liftIO $ connect localPG
      liftIO $ withTransaction conn (do
          mVisit <- query conn visitExistsQuery (VP.eventId v, VP.visitorId v, VP.status v, VP.plusOne v)
          case mVisit of
            (visit:_) -> return visit
            _ -> do
              execute conn markVisitsAsSupersededQuery (VP.eventId v, VP.visitorId v)
              [newVisit] <- query conn insertVisitQuery (VP.eventId v, VP.visitorId v, VP.status v, VP.plusOne v)
              return newVisit
        )

visitExistsQuery = "select event_id, visitor_id, status, plus_one, rsvp_at \
                   \from visits \
                   \where \
                   \  event_id = ? \
                   \  and visitor_id = ? \
                   \  and status = ? \
                   \  and plus_one = ? \
                   \  and superseded_at is null"

markVisitsAsSupersededQuery = "update visits \
                              \set superseded_at = now() \
                              \where \
                              \  superseded_at is null \
                              \    and \
                              \    event_id = ? \
                              \    and visitor_id = ?"

insertVisitQuery = "insert into visits (event_id, visitor_id, status, plus_one) \
                   \values (?, ?, ?, ?) \
                   \returning event_id, visitor_id, status, plus_one, rsvp_at"

