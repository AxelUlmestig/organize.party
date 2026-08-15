module Op.Aws (AwsEnv (..), HasAwsEnv (..), presignUploadUrl, loadAwsEnvFromEnvVars, PresignS3PutUrlArguments(..), photoObjectKey, objectExists) where

import qualified Amazonka                  as AWS
import qualified Amazonka.S3               as AWS.S3
import           Control.Monad.Catch       (MonadCatch)
import qualified Data.Text                 as Text
import           Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import           Data.Time.Clock           (UTCTime)
import           Data.UUID                 (UUID)
import qualified Data.UUID                 as UUID
import           Network.HTTP.Types.Status (statusCode)
import           RIO
import           System.Environment        (lookupEnv)

data AwsEnv = AwsEnv
  { awsEnv   :: AWS.Env
  , s3Bucket :: Text
  }

class HasAwsEnv a where
  getAwsEnv :: a -> AwsEnv

instance HasAwsEnv AwsEnv where
  getAwsEnv = id

-- | AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, and AWS_REGION env variables needed here
--
-- S3_ENDPOINT optionally points at an S3 compatible endpoint other than AWS,
-- amazonka can only derive endpoints from the region. S3_BUCKET names the
-- bucket, defaulting to the previously hardcoded one.
loadAwsEnvFromEnvVars :: (MonadCatch m, MonadIO m) => m AwsEnv
loadAwsEnvFromEnvVars = do
  env <- AWS.newEnv AWS.discover
  mEndpoint <- liftIO $ lookupEnv "S3_ENDPOINT"
  mBucket <- liftIO $ lookupEnv "S3_BUCKET"

  let overridden = case mEndpoint of
        Nothing       -> env
        Just endpoint -> AWS.overrideService (setS3Endpoint (Text.pack endpoint)) env

  pure AwsEnv
    { awsEnv = overridden
    , s3Bucket = maybe "organize-party" Text.pack mBucket
    }

-- path style addressing since a bucket as a subdomain needs a wildcard
-- certificate that the endpoint might not have
setS3Endpoint :: Text -> AWS.Service -> AWS.Service
setS3Endpoint endpoint service =
  (AWS.setEndpoint secure (encodeUtf8 host) port service)
    { AWS.s3AddressingStyle = AWS.S3AddressingStylePath }
  where
    (secure, authority) =
      case Text.stripPrefix "http://" endpoint of
        Just rest -> (False, rest)
        Nothing   -> (True, fromMaybe endpoint (Text.stripPrefix "https://" endpoint))

    (host, portSuffix) = Text.breakOn ":" (Text.takeWhile (/= '/') authority)

    port =
      fromMaybe
        (if secure then 443 else 80)
        (Text.stripPrefix ":" portSuffix >>= readMaybe . Text.unpack)

data PresignS3PutUrlArguments =
  PresignS3PutUrlArguments
  { objectKey       :: Text -- AWS.S3.ObjectKey
  , currentTime     :: UTCTime
  , urlValidSeconds :: Int
  }

presignUploadUrl ::
  ( MonadIO m
  , MonadReader env m
  , HasAwsEnv env
  )
  => PresignS3PutUrlArguments
  -> m Text
presignUploadUrl PresignS3PutUrlArguments{..} = do
  AwsEnv{awsEnv, s3Bucket} <- asks getAwsEnv

  let s3PutRequestBody = AWS.toBody ("" :: ByteString)
  let putObject = AWS.S3.newPutObject (AWS.S3.BucketName s3Bucket) (AWS.S3.ObjectKey objectKey) s3PutRequestBody

  uploadUrlByteString <-
    AWS.presignURL
      awsEnv
      currentTime
      (fromIntegral urlValidSeconds)
      putObject

  pure $ decodeUtf8 uploadUrlByteString

-- | The webapi presigns an upload url for this key and the worker later polls
-- it, so they need to agree on it
photoObjectKey :: UUID -> Text -> Text
photoObjectKey photoUploadId fileName =
  "photos/" <> UUID.toText photoUploadId <> "/" <> fileName

-- | Check if the object is in the bucket with a HEAD request under our own
-- credentials. An anonymous request can't tell "not there" from "not yours",
-- a private bucket answers 403 to both
objectExists ::
  ( MonadIO m
  , MonadReader env m
  , HasAwsEnv env
  )
  => Text
  -> m Bool
objectExists objectKey = do
  AwsEnv{awsEnv, s3Bucket} <- asks getAwsEnv

  let headObject = AWS.S3.newHeadObject (AWS.S3.BucketName s3Bucket) (AWS.S3.ObjectKey objectKey)

  liftIO do
    result <- try $ AWS.runResourceT $ AWS.send awsEnv headObject

    case result of
      Right _ -> pure True
      Left (AWS.ServiceError AWS.ServiceError'{status})
        | statusCode status == 404 -> pure False
      Left err -> throwIO err
