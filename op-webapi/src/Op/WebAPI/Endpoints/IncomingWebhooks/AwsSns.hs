{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.IncomingWebhooks.AwsSns (handleAwsSnsWebhook) where

import           Control.Monad.Except    (MonadError (..))
import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Control.Monad.Reader    (MonadReader, asks)
import qualified Data.Aeson              as Aeson
-- import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy    as LBS
import           Data.Profunctor         (dimap)
import           Data.String.Interpolate (i)
import           Data.Text               (Text)
import           Data.Text.Encoding      (encodeUtf8)
import           Data.Types.Isomorphic   (to)
import qualified Hasql.Session           as Hasql
import           Hasql.Statement         (Statement)
import           Hasql.TH                (resultlessStatement)
-- import           Lens.Micro              ((^?))
{-
import           Network.HTTP.Req        (GET (GET), NoReqBody (NoReqBody), req,
                                          useHttpsURI)
                                          -}
import           Servant                 (ServerError (..), err500)
{-
import           Text.URI                (mkURI)

-}
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

  {-
  Db.queryDbOr undefined (Db.statement insertStatement requestBody)
  where
    insertStatement =
      [Db.resultlessStatement|
        insert into aws.ses_webhooks (
          content
        )
        values ($1::jsonb)
      |]
  -}

  {-
  let mSubscribeUrl = do
        textUrl <- requestBody ^? key "SubscribeURL" . _String
        uri <- mkURI textUrl
        useHttpsURI uri

  case (requestBody ^? key "Type" . _String, mSubscribeUrl) of
    (Just "SubscriptionConfirmation", Just (subscribeUrl, options)) -> do
      req
        GET
        subscribeUrl
        NoReqBody
    _ -> do
      let onErr err = do
            liftIO $ print err
            throwError err500 { errBody = "Something went wrong" }

      let statement =
            [resultlessStatement|
              insert into aws_ses_webhooks (
                content
              )
              values ($1::jsonb)
            |]

      Db.queryDb Db.printAndThrow500 statement requestBody
      -}



{-
queryDb
  :: (MonadError ServerError m, MonadIO m, MonadReader AppEnv m)
  => (Hasql.SessionError -> m b)
  -> Statement a b
  -> a
  -> m b
queryDb onErr statement input = do
  conn <- asks connection
  eResult <- liftIO $ Hasql.run (Hasql.statement input statement) conn
  case eResult of
    Left err    -> onErr err
    Right event -> pure event
-}

