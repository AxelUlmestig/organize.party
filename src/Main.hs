{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Reader       (ReaderT (..))
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import           Data.ByteString.UTF8             as BSU
import           Data.Int                         (Int64)
import           Data.Text                        (Text, pack)
import           Data.UUID                        (UUID)
import           Hasql.Connection                 (Connection, Settings,
                                                   acquire, settings)
import qualified Hasql.Session                    as Hasql
import           Hasql.Statement                  (Statement)
import           Hasql.TH                         (maybeStatement,
                                                   resultlessStatement,
                                                   singletonStatement)
import           Network.HTTP.Media               ((//), (/:))
import           Network.Wai.Handler.Warp         (run)
import           Network.Wai.Middleware.Cors      (simpleCors)
import           Servant
import           Servant.API
import           System.Environment               (lookupEnv)
import           System.Exit                      (die)
import           Text.Read                        (readMaybe)

import qualified Email
import qualified Endpoints.Attend
import qualified Endpoints.Comment
import qualified Endpoints.CreateEvent
import qualified Endpoints.EditEvent
import qualified Endpoints.ExecuteForgetMeRequest
import qualified Endpoints.GetEvent
import qualified Endpoints.InitForgetMeRequest
import qualified Endpoints.ViewForgetMeRequest
import           Types.AppEnv                     (AppEnv (..), SmtpConfig (..))
import           Types.Attendee                   (Attendee)
import           Types.AttendInput                (AttendInput)
import           Types.CommentInput               (CommentInput)
import           Types.CreateEventInput           (CreateEventInput)
import           Types.Event                      (Event)
import           Types.ForgetMeRequest            (ExecuteForgetMeResult (..),
                                                   ForgetMeRequest (..),
                                                   InitForgetMeInput (..),
                                                   InitForgetMeResult (..))

type API
  = GetEventAPI
  :<|> EditEventAPI
  :<|> CreateEventAPI
  :<|> AttendeesAPI
  :<|> CommentAPI
  :<|> InitForgetMeRequestApi
  :<|> ViewForgetMeRequestApi
  :<|> ExecuteForgetMeRequestApi
  :<|> CreateEventHtml
  :<|> ViewEventHtml
  :<|> EditEventHtml
  :<|> AboutHtml
  :<|> ForgetMeEventHtml
  :<|> ViewForgetMeEventHtml
  :<|> Raw

type CreateEventAPI = "api" :> "v1" :> "events" :> ReqBody '[JSON] CreateEventInput :> Post '[JSON] Event
type GetEventAPI = "api" :> "v1" :> "events" :> Capture "event_id" UUID :> Get '[JSON] Event
type EditEventAPI = "api" :> "v1" :> "events" :> Capture "event_id" UUID :> "edit" :> ReqBody '[JSON] CreateEventInput :> Put '[JSON] Event
type AttendeesAPI = "api" :> "v1" :> "events" :> Capture "event_id" UUID :> "attend" :> ReqBody '[JSON] AttendInput :> Put '[JSON] Event
type CommentAPI = "api" :> "v1" :> "events" :> Capture "event_id" UUID :> "comment" :> ReqBody '[JSON] CommentInput :> Post '[JSON] Event

type InitForgetMeRequestApi = "api" :> "v1" :> "forget-me" :> ReqBody '[JSON] InitForgetMeInput :> Put '[JSON] InitForgetMeResult
type ViewForgetMeRequestApi = "api" :> "v1" :> "forget-me" :> Capture "forgetme_request_id" UUID :> Get '[JSON] ForgetMeRequest
type ExecuteForgetMeRequestApi = "api" :> "v1" :> "forget-me" :> Capture "forgetme_request_id" UUID :> Delete '[JSON] ExecuteForgetMeResult

type CreateEventHtml = Get '[HTML] RawHtml
type ViewEventHtml = "e" :> Capture "event_id" UUID :> Get '[HTML] RawHtml
type EditEventHtml = "e" :> Capture "event_id" UUID :> "edit" :> Get '[HTML] RawHtml
type AboutHtml = "about" :> Get '[HTML] RawHtml
type ForgetMeEventHtml = "forget-me" :> Get '[HTML] RawHtml
type ViewForgetMeEventHtml = "forget-me" :> Capture "forgetme_request_id" UUID :> Get '[HTML] RawHtml

type MyHandler = ReaderT AppEnv Handler

api :: Proxy API
api = Proxy

app :: AppEnv -> Application
app env = simpleCors . serve api $ hoistServer api (flip runReaderT env) servantServer
  where
    servantServer =
        Endpoints.GetEvent.getEvent
        :<|> Endpoints.EditEvent.editEvent
        :<|> Endpoints.CreateEvent.createEvent
        :<|> Endpoints.Attend.attend
        :<|> Endpoints.Comment.addComment
        :<|> Endpoints.InitForgetMeRequest.initForgetMe
        :<|> Endpoints.ViewForgetMeRequest.viewForgetMeRequest
        :<|> Endpoints.ExecuteForgetMeRequest.executeForgetMeRequest
        :<|> frontPage
        :<|> eventPage -- view event
        :<|> eventPage -- edit event
        :<|> frontPage -- about
        :<|> frontPage -- forget me
        :<|> eventPage -- forget me id
        :<|> serveDirectoryWebApp "frontend/static"
      where
        frontPage = fmap RawHtml (liftIO $ LBS.readFile "frontend/index.html")
        eventPage _ = fmap RawHtml (liftIO $ LBS.readFile "frontend/index.html")

getDbSettings :: IO (Either String Settings)
getDbSettings = do
    mHost <- fmap BSU.fromString <$> lookupEnv "DB_HOST"
    mPort <- lookupEnv "DB_PORT"
    pure do
      host <- maybeToEither "Error: Missing env variable DB_HOST" mHost
      port <- maybeToEither "Error: Missing env variable DB_PORT" mPort >>= maybeToEither "Error: Couldn't parse port from DB_PORT" . readMaybe

      pure $ settings host port "postgres" "postgres" "events"

getSmtpConfig :: IO (Either String SmtpConfig)
getSmtpConfig = do
  mServer <- lookupEnv "SMTP_SERVER"
  mPort <- lookupEnv "SMTP_PORT"
  mLogin <- lookupEnv "SMTP_LOGIN"
  mPassword <- lookupEnv "SMTP_PASSWORD"
  pure do
    server <- maybeToEither "Error: Missing env variable SMTP_SERVER" mServer
    port <- maybeToEither "Error: Missing env variable SMTP_PORT" mPort >>= maybeToEither "Error: Couldn't parse port from SMTP_PORT" . readMaybe
    login <- maybeToEither "Error: Missing env variable SMTP_LOGIN" mLogin
    password <- maybeToEither "Error: Missing env variable SMTP_PASSWORD" mPassword
    pure SmtpConfig {server, port, login, password}

getHostUrl :: IO (Either String String)
getHostUrl = do
  mHostUrl <- lookupEnv "HOST_URL"
  pure $ maybeToEither "Error: Missing env variable HOST_URL" mHostUrl

main :: IO ()
main = do
  dbSettings <- getDbSettings >>= either die pure
  smtpConfig <- getSmtpConfig >>= either die pure
  hostUrl <- getHostUrl >>= either die pure
  eConnection <- acquire dbSettings
  case eConnection of
    Left err -> print err
    Right connection -> do
      putStrLn "listening on port 8081..."
      run 8081 $ app AppEnv { connection, smtpConfig, hostUrl }

-- type shenanigans to enable serving raw html

data HTML

newtype RawHtml = RawHtml { unRaw :: LBS.ByteString }

instance Accept HTML where
  contentTypes _ = pure $ "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

-- util
maybeToEither _ (Just a)  = Right a
maybeToEither err Nothing = Left err
