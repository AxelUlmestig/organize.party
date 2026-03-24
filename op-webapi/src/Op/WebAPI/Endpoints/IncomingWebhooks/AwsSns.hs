{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.IncomingWebhooks.AwsSns (handleAwsSnsWebhook) where

import           Control.Monad.Except     (MonadError (..))
import qualified Control.Monad.Except     as Except
import           Crypto.Hash.Algorithms   (SHA1 (..), SHA256 (..))
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import           Crypto.PubKey.RSA.Types  (PublicKey (..))
import qualified Data.Aeson               as Aeson
import           Data.Aeson.Casing        (aesonPrefix, pascalCase)
import qualified Data.ByteString.Base64   as B64
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.PEM                 as PEM
import           Data.String.Interpolate  (__i, i)
import qualified Data.Text                as Text
import qualified Data.X509                as X509
import qualified Network.HTTP.Req         as Req
import           RIO
import           Servant                  (ServerError (..), err400)
import           Text.Regex.TDFA          ((=~))
import           Text.URI                 (mkURI)

import qualified Op.Cache                 as Cache
import qualified Op.Db                    as Db
import           Op.WebAPI.Types.AppEnv   (AppEnv (..))

handleAwsSnsWebhook
  :: (MonadError ServerError m, MonadIO m, MonadReader AppEnv m)
  => Text
  -> m ()
handleAwsSnsWebhook rawRequestBody = do
  requestBodyJson <- do
    case Aeson.decode $ LBS.fromStrict $ encodeUtf8 rawRequestBody of
      Just x -> pure x
      Nothing -> do
        logWarn [i|Unexpected AWS SES request, expected JSON but got: #{rawRequestBody}|]
        throwError err400 { errBody = [i|Unexpected AWS SES request, expected JSON but got: #{rawRequestBody}|] }

  requestBody <- do
    case Aeson.fromJSON requestBodyJson of
      Aeson.Success requestBody -> pure requestBody
      Aeson.Error err -> do
        logWarn [i|Failed to parse AWS SES request body: #{rawRequestBody}. Error: #{err}|]
        throwError err400 { errBody = "Invalid request body" }

  -- verify signature
  do
    signature <- do
      case B64.decode . encodeUtf8 . whSignature $ requestBody of
        Left err -> do
          logWarn [i|Couldn't base64 decode 'Signature' in AWS SNS webhook request: #{err}|]
          throwError err400 { errBody = "Couldn't base64 decode 'Signature' in request body" }
        Right signature -> pure signature

    pubKey <- do
      let certUrl = whSigningCertURL requestBody
      Cache.getCachedM certUrl do
        ePubKey <- getSignaturePublicKey certUrl

        case ePubKey of
          Left err -> do
            logWarn [i|Failed to parse AWS SNS Webhook certificate: #{err}|]
            throwError err400 { errBody = "Failed to extract signature cert" }
          Right pubKey -> pure pubKey

    let messageToVerify = constructMessageToVerify requestBody

    let signatureValid =
          case whSignatureVersion requestBody of
            Sha1Signature ->
              RSA.verify
                (Just SHA1)
                pubKey
                messageToVerify
                signature
            Sha256Signature ->
              RSA.verify
                (Just SHA256)
                pubKey
                messageToVerify
                signature

    unless signatureValid do
      logWarn "Invalid AWS SNS signature"
      throwError err400 { errBody = "Invalid signature" }

  Db.queryDbOr Db.printAndThrow500 do
    Db.statement
      (requestBodyJson, whMessageId requestBody)
      [Db.resultlessStatement|
      insert into aws.sns_webhook_messages (contents, aws_sns_id)
      values ($1::jsonb, $2::text)
      |]

getSignaturePublicKey
  :: (MonadIO m, MonadReader AppEnv m)
  => Text
  -> m (Either String PublicKey)
getSignaturePublicKey rawSigningCertUrl = do
  Except.runExceptT do
    -- verify that signing cert url comes from AWS
    do
      let awsDomainRegex = "^https:\\/\\/([a-z0-9-]*\\.)*amazonaws\\.com.*$" :: Text
      let localhostDomainRegex = "^http:\\/\\/localhost:8888(\\/.*)$" :: Text -- allow localhost for testing
      unless (rawSigningCertUrl =~ awsDomainRegex || rawSigningCertUrl =~ localhostDomainRegex) do
        -- runRIO appEnv do
        logWarn [i|Illegal signing cert url in AWS SNS webhook: #{rawSigningCertUrl}|]
        Except.throwError "Illegal signing cert url. Only amazonaws.com is allowed"

    response <- do
      case Req.useURI =<< mkURI rawSigningCertUrl of
        Nothing  -> Except.throwError [i|Couldn't parse AWS SNS signing cert url: #{rawSigningCertUrl}|]
        Just (Left url) -> downloadCert url
        Just (Right url) -> downloadCert url

    case Req.responseStatusCode response `div` 100 of
      2 -> pure ()
      _ -> Except.throwError [i|Unexpected HTTP response code when calling AWS SNS signing cert url: #{Req.responseStatusCode response}|]

    pem <- do
      case PEM.pemParseBS (Req.responseBody response) of
        Right [pem] -> pure pem
        Right pems -> Except.throwError [i|Unexpected AWS SNS signing cert PEM: #{pems}|]
        Left err -> Except.throwError [i|Couldn't parse AWS SNS signing cert .pem: #{err}|]

    signedCertificatePubKey <- do
      case X509.decodeSignedCertificate (PEM.pemContent pem) of
        Left err -> Except.throwError [i|Couldn't decode AWS SNS signed cert from .pem: #{err}|]
        Right signedCert -> pure . X509.certPubKey . X509.signedObject . X509.getSigned $ signedCert

    case signedCertificatePubKey of
      X509.PubKeyRSA rsaPubKey -> pure rsaPubKey
      _ -> Except.throwError [i|Unexpected AWS SNS signing cert pub key: #{signedCertificatePubKey}|]
  where
    downloadCert :: forall a m. MonadIO m => (Req.Url a, Req.Option a) -> m Req.BsResponse
    downloadCert (url, options) = do
      Req.runReq Req.defaultHttpConfig do
        Req.req
          Req.GET
          url
          Req.NoReqBody
          Req.bsResponse
          options

-- | The AWS documentation stresses that there shouldn't be a newline at the
-- end. But it only works when I purposefully add a newline
--
-- https://docs.aws.amazon.com/sns/latest/dg/sns-verify-signature-of-message-verify-message-signature.html
constructMessageToVerify :: SnsWebhook -> ByteString
constructMessageToVerify SnsWebhook{..} =
    [__i|
      Message
      #{whMessage}
      MessageId
      #{whMessageId}#{subject}#{subscribeUrl}
      Timestamp
      #{whTimestamp}#{token}
      TopicArn
      #{whTopicArn}
      Type
      #{whType}\n
    |]
  where
    subject =
      case whSubject of
        Nothing -> ""
        Just s ->
          [__i|
          \nSubject
          #{s}
          |] :: Text
    subscribeUrl =
      case whSubscribeURL of
        Nothing -> "" :: Text
        Just s ->
          [__i|
          \nSubscribeURL
          #{s}
          |] :: Text
    token =
      case whToken of
        Nothing -> ""
        Just t ->
          [__i|
          \nToken
          #{t}
          |] :: Text

data SnsWebhook = SnsWebhook
  { whType             :: Text
  , whToken            :: Maybe Text
  , whMessage          :: Text
  , whSubject          :: Maybe Text
  , whTopicArn         :: Text
  , whMessageId        :: Text
  , whSignature        :: Text
  , whTimestamp        :: Text
  , whSubscribeURL     :: Maybe Text
  , whSigningCertURL   :: Text
  , whSignatureVersion :: SnsWebhookSignatureVersion
  } deriving (Generic, Show)

instance Aeson.FromJSON SnsWebhook where
   parseJSON = Aeson.genericParseJSON $ aesonPrefix pascalCase

data SnsWebhookSignatureVersion
  = Sha1Signature
  | Sha256Signature
  deriving (Generic, Show)

instance Aeson.FromJSON SnsWebhookSignatureVersion where
  parseJSON = Aeson.withText "SnsWebhookSignatureVersion" $ \t ->
    case t of
      "1" -> pure Sha1Signature
      "2" -> pure Sha256Signature
      _   -> fail $ "Invalid signature version: " ++ Text.unpack t

{-
instance Aeson.ToJSON SnsWebhook where
  toJSON = Aeson.genericToJSON $ aesonPrefix pascalCase

instance Aeson.ToJSON SnsWebhookSignatureVersion where
  toJSON Sha1Signature   = Aeson.String "1"
  toJSON Sha256Signature = Aeson.String "2"
-}
