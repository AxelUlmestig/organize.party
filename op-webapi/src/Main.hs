{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Data.ByteString.UTF8                        as BSU
import           Data.String.Interpolate                     (i)
import qualified Data.Text                                   as Text
import           Data.UUID                                   (UUID)
import           Network.Wai.Handler.Warp                    (run)
import           Network.Wai.Middleware.Cors                 (simpleCors)
import           RIO
import           Servant
import           System.Environment                          (lookupEnv)
import           System.Exit                                 (die)

import qualified Op.Aws                                      as Aws
import qualified Op.Cache                                    as Cache
import qualified Op.Db                                       as Db
import qualified Op.WebAPI.Endpoints.Attend
import qualified Op.WebAPI.Endpoints.Comment
import qualified Op.WebAPI.Endpoints.CreateEvent
import qualified Op.WebAPI.Endpoints.EditEvent
import qualified Op.WebAPI.Endpoints.ExecuteForgetMeRequest
import qualified Op.WebAPI.Endpoints.GetEvent
import qualified Op.WebAPI.Endpoints.GetPhotoUpload
import qualified Op.WebAPI.Endpoints.IncomingWebhooks.AwsSns
import qualified Op.WebAPI.Endpoints.InitForgetMeRequest
import           Op.WebAPI.Endpoints.InitPhotoUpload         (InitPhotoUploadInput,
                                                              InitPhotoUploadResult)
import qualified Op.WebAPI.Endpoints.InitPhotoUpload
import qualified Op.WebAPI.Endpoints.Unsubscribe
import qualified Op.WebAPI.Endpoints.ViewForgetMeRequest
import           Op.WebAPI.Html                              (HTML, RawHtml,
                                                              eventPage,
                                                              frontPage)
import           Op.WebAPI.Types.AppEnv                      (AppEnv (..))
import           Op.WebAPI.Types.AttendInput                 (AttendInput)
import           Op.WebAPI.Types.CommentInput                (CommentInput)
import           Op.WebAPI.Types.CreateEventInput            (CreateEventInput)
import           Op.WebAPI.Types.Event                       (Event)
import           Op.WebAPI.Types.ForgetMeRequest             (ExecuteForgetMeResult (..),
                                                              ForgetMeRequest (..),
                                                              InitForgetMeInput (..),
                                                              InitForgetMeResult (..))
import           Op.WebAPI.Types.Unsubscribe                 (UnsubscribeResult)

type API
  = GetEventAPI
  :<|> EditEventAPI
  :<|> CreateEventAPI
  :<|> AttendeesAPI
  :<|> CommentAPI
  :<|> InitForgetMeRequestApi
  :<|> ViewForgetMeRequestApi
  :<|> AwsSnsWebhookApi
  :<|> InitPhotoUploadApi
  :<|> GetPhotoUploadApi
  :<|> ExecuteForgetMeRequestApi
  :<|> UnsubscribeApi
  :<|> CreateEventHtml
  :<|> ViewEventHtml
  :<|> EditEventHtml
  :<|> AboutHtml
  :<|> ForgetMeEventHtml
  :<|> ViewForgetMeEventHtml
  :<|> UnsubscribeHtml
  :<|> Raw

type CreateEventAPI = "api" :> "v1" :> "events" :> ReqBody '[JSON] CreateEventInput :> Post '[JSON] Event
type GetEventAPI = "api" :> "v1" :> "events" :> Capture "event_id" UUID :> Get '[JSON] Event
type EditEventAPI = "api" :> "v1" :> "events" :> Capture "event_id" UUID :> "edit" :> ReqBody '[JSON] CreateEventInput :> Put '[JSON] Event
type AttendeesAPI = "api" :> "v1" :> "events" :> Capture "event_id" UUID :> "attend" :> ReqBody '[JSON] AttendInput :> Put '[JSON] Event
type CommentAPI = "api" :> "v1" :> "events" :> Capture "event_id" UUID :> "comment" :> ReqBody '[JSON] CommentInput :> Post '[JSON] Event

type InitForgetMeRequestApi = "api" :> "v1" :> "forget-me" :> ReqBody '[JSON] InitForgetMeInput :> Put '[JSON] InitForgetMeResult
type ViewForgetMeRequestApi = "api" :> "v1" :> "forget-me" :> Capture "forgetme_request_id" UUID :> Get '[JSON] ForgetMeRequest
type ExecuteForgetMeRequestApi = "api" :> "v1" :> "forget-me" :> Capture "forgetme_request_id" UUID :> Delete '[JSON] ExecuteForgetMeResult

type UnsubscribeApi = "api" :> "v1" :> "unsubscribe" :> Capture "unsubscribe_id" UUID :> Put '[JSON] UnsubscribeResult

type AwsSnsWebhookApi = "api" :> "v1" :> "incoming-webhook" :> "aws-sns" :> ReqBody '[PlainText] Text :> Post '[JSON] ()

type InitPhotoUploadApi = "api" :> "v1" :> "photo-upload" :> ReqBody '[JSON] InitPhotoUploadInput :> Post '[JSON] (InitPhotoUploadResult Maybe)
type GetPhotoUploadApi = "api" :> "v1" :> "photo-upload" :> Capture "photo_upload_id" UUID :> Get '[JSON] (InitPhotoUploadResult Identity)

type CreateEventHtml = Get '[HTML] RawHtml
type ViewEventHtml = "e" :> Capture "event_id" UUID :> Get '[HTML] RawHtml
type EditEventHtml = "e" :> Capture "event_id" UUID :> "edit" :> Get '[HTML] RawHtml
type AboutHtml = "about" :> Get '[HTML] RawHtml
type ForgetMeEventHtml = "forget-me" :> Get '[HTML] RawHtml
type ViewForgetMeEventHtml = "forget-me" :> Capture "forgetme_request_id" UUID :> Get '[HTML] RawHtml
type UnsubscribeHtml = "unsubscribe" :> Capture "unsubscribe_id" UUID :> Get '[HTML] RawHtml

type MyHandler = ReaderT AppEnv Servant.Handler

api :: Proxy API
api = Proxy

app :: AppEnv -> Application
app env = simpleCors . serve api $ hoistServer api (`runReaderT` env) servantServer
  where
    servantServer =
        Op.WebAPI.Endpoints.GetEvent.getEvent
        :<|> Op.WebAPI.Endpoints.EditEvent.editEvent
        :<|> Op.WebAPI.Endpoints.CreateEvent.createEvent
        :<|> Op.WebAPI.Endpoints.Attend.attend
        :<|> Op.WebAPI.Endpoints.Comment.addComment
        :<|> Op.WebAPI.Endpoints.InitForgetMeRequest.initForgetMe
        :<|> Op.WebAPI.Endpoints.ViewForgetMeRequest.viewForgetMeRequest
        :<|> Op.WebAPI.Endpoints.IncomingWebhooks.AwsSns.handleAwsSnsWebhook
        :<|> Op.WebAPI.Endpoints.InitPhotoUpload.initPhotoUpload
        :<|> Op.WebAPI.Endpoints.GetPhotoUpload.getPhotoUpload
        :<|> Op.WebAPI.Endpoints.ExecuteForgetMeRequest.executeForgetMeRequest
        :<|> Op.WebAPI.Endpoints.Unsubscribe.unsubscribe
        :<|> frontPage
        :<|> eventPage -- view event
        :<|> eventPage -- edit event
        :<|> frontPage -- about
        :<|> frontPage -- forget me
        :<|> const frontPage -- forget me id
        :<|> const frontPage -- unsubscribe id
        :<|> serveDirectoryWebApp "frontend/static"

getDbConnectionSettings :: IO (Either String Db.Settings)
getDbConnectionSettings = do
    mHost <- fmap BSU.fromString <$> lookupEnv "DB_HOST"
    mPort <- lookupEnv "DB_PORT"
    pure do
      host <- maybeToEither "Error: Missing env variable DB_HOST" mHost
      port :: Int <- maybeToEither "Error: Missing env variable DB_PORT" mPort >>= maybeToEither "Error: Couldn't parse port from DB_PORT" . readMaybe

      let connectionString = [i|host=#{host} dbname=events user=postgres password=postgres port=#{port}|]
      pure $ Db.connectionString connectionString


getHostUrl :: IO (Either String Text)
getHostUrl = do
  mHostUrl <- fmap Text.pack <$> lookupEnv "HOST_URL"
  pure $ maybeToEither "Error: Missing env variable HOST_URL" mHostUrl

main :: IO ()
main = do
  withLogFunction $ \logFunc -> do
    dbSettings <- getDbConnectionSettings >>= either die pure
    hostUrl <- getHostUrl >>= either die pure
    connectionPool <- Db.createPool dbSettings
    awsSnsPubKeyCache <- Cache.initCache
    awsEnv <- Aws.loadAwsEnvFromEnvVars
    let port = 8081

    runRIO logFunc do
      logInfo [i|listening on port #{port}...|]

    run port $ app AppEnv {..}

-- util
maybeToEither :: err -> Maybe a -> Either err a
maybeToEither _ (Just a)  = Right a
maybeToEither err Nothing = Left err

withLogFunction :: MonadUnliftIO m => (LogFunc -> m a) -> m a
withLogFunction action = do
  logLevel <- getLogLevelFromEnv
  logOptions <- setLogUseTime True
                  . setLogUseLoc True
                  . setLogMinLevel logLevel
                  <$> logOptionsHandle stderr True
  withLogFunc logOptions action
  where
    getLogLevelFromEnv = do
      mLogLevel <- liftIO $ lookupEnv "LOG_LEVEL" <&> fmap readMaybe
      case mLogLevel of
        Nothing              -> pure LevelInfo
        Just Nothing         -> error "Couldn't parse LOG_LEVEL argument"
        Just (Just logLevel) -> pure logLevel

