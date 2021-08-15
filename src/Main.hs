{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.IO.Class     (liftIO)
import           Database.PostgreSQL.Simple (ConnectInfo (..), connect,
                                             defaultConnectInfo, query_)
import           Network.Wai.Handler.Warp   (run)
import           Servant                    (Application, Proxy (Proxy), Server,
                                             serve)
import           Servant.API                (Get, JSON, (:>))

import           Types.Event                (Event)
import qualified Types.Event                as E
import           Types.Visitor              (Visitor)
import qualified Types.Visitor              as V

localPG :: ConnectInfo
localPG = defaultConnectInfo
        { connectHost = "localhost"
        , connectDatabase = "events"
        , connectUser = "postgres"
        , connectPassword = "postgres"
        }

type EventAPI = "api" :> "v1" :> "events" :> Get '[JSON] [Event]

eventAPI :: Proxy EventAPI
eventAPI = Proxy

app :: Application
app = serve eventAPI server

main :: IO ()
main = do
  putStrLn "listening on port 8081..."
  run 8081 app

-- main :: IO ()
-- main = do
--   conn <- connect localPG
--   mapM_ print =<< (query_ conn "select id, email::text, first_name, last_name from visitors" :: IO [Visitor])
--   mapM_ print =<< (query_ conn "select * from events" :: IO [Event])

server :: Server EventAPI
server = do
  conn <- liftIO $ connect localPG
  liftIO $ query_ conn "select * from events"
