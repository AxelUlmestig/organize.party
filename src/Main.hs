{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.IO.Class     (liftIO)
import           Database.PostgreSQL.Simple (ConnectInfo (..), connect,
                                             defaultConnectInfo, query, query_)
import           Network.Wai.Handler.Warp   (run)
import           Servant
import           Servant.API



import           Types.Event                (Event)
import qualified Types.Event                as E
import           Types.Visitor              (Visitor)
import qualified Types.Visitor              as V

localPG :: ConnectInfo
localPG = defaultConnectInfo
        { connectHost = "localhost"
        , connectPort = 5433
        , connectDatabase = "events"
        , connectUser = "postgres"
        , connectPassword = "postgres"
        }

type API = EventAPI :<|> VisitorAPI
type EventAPI = "api" :> "v1" :> "events" :> Get '[JSON] [Event]
type VisitorAPI = "api" :> "v1" :> "visitors" :> QueryParam "email" String :> Get '[JSON] Visitor

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = do
  putStrLn "listening on port 8081..."
  run 8081 app

server :: Server API
server = events
    :<|> visitors

  where
    events = do
      conn <- liftIO $ connect localPG
      liftIO $ query_ conn "select * from events"

    visitors :: Maybe String -> Handler Visitor
    visitors Nothing = throwError $ err400 { errBody = "supply an 'email' query param" }
    visitors (Just email) = do
      conn <- liftIO $ connect localPG
      rows <- liftIO $ query conn "select id, email::text, first_name, last_name from visitors where email = ?" [email]
      case rows of
        (visitor:_) -> return visitor
        _           -> throwError $ err404 { errBody = "visitor not found" }
