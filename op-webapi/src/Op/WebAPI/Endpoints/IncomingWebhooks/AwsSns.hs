{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.IncomingWebhooks.AwsSns (handleAwsSnsWebhook) where

import           Control.Monad.Except    (MonadError (..))
import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Control.Monad.Reader    (MonadReader)
import qualified Data.Aeson              as Aeson
import qualified Data.ByteString.Lazy    as LBS
import           Data.String.Interpolate (i)
import           Data.Text               (Text)
import           Data.Text.Encoding      (encodeUtf8)
import           Servant                 (ServerError (..), err500)

import qualified Op.Db                   as Db
import           Op.WebAPI.Types.AppEnv  (AppEnv (..))

handleAwsSnsWebhook
  :: (MonadError ServerError m, MonadIO m, MonadReader AppEnv m)
  => Text
  -> m ()
handleAwsSnsWebhook rawRequestBody = do
  -- TODO: verify that it's actually coming from AWS

  requestBody :: Aeson.Value <- do
      case Aeson.decode $ LBS.fromStrict $ encodeUtf8 rawRequestBody of
        Just x -> pure x
        Nothing -> do
          let errorMessage = [i|Unexpected AWS SES request, expected JSON but got: #{rawRequestBody}|]
          liftIO $ print errorMessage
          throwError err500 { errBody = errorMessage }

  liftIO $ putStrLn [i|Received webhook from AWS SES: #{Aeson.encode requestBody}|]

  Db.queryDbOr Db.printAndThrow500 do
    Db.statement
      requestBody
      [Db.resultlessStatement|
      insert into aws.sns_webhook_messages (contents)
      values ($1::jsonb)
      |]
