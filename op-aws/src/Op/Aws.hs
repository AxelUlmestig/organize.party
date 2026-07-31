module Op.Aws (AwsEnv, HasAwsEnv (..), presignUploadUrl, loadAwsEnvFromEnvVars, PresignS3PutUrlArguments(..)) where

import qualified Amazonka            as AWS
import qualified Amazonka.S3         as AWS.S3
import           Control.Monad.Catch (MonadCatch)
import           Data.Text.Encoding  (decodeUtf8)
import           Data.Time.Clock     (UTCTime)
import           RIO

newtype AwsEnv = AwsEnv AWS.Env

class HasAwsEnv a where
  getAwsEnv :: a -> AwsEnv

instance HasAwsEnv AwsEnv where
  getAwsEnv = id

-- | AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, and AWS_REGION env variables needed here
loadAwsEnvFromEnvVars :: (MonadCatch m, MonadIO m) => m AwsEnv
loadAwsEnvFromEnvVars = AwsEnv <$> AWS.newEnv AWS.discover

data PresignS3PutUrlArguments =
  PresignS3PutUrlArguments
  { objectKey       :: Text -- AWS.S3.ObjectKey
  , bucketName      :: Text
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
  AwsEnv awsEnv <- asks getAwsEnv

  let s3PutRequestBody = AWS.toBody ("" :: ByteString)
  let putObject = AWS.S3.newPutObject (AWS.S3.BucketName bucketName) (AWS.S3.ObjectKey objectKey) s3PutRequestBody

  uploadUrlByteString <-
    AWS.presignURL
      awsEnv
      currentTime
      (fromIntegral urlValidSeconds)
      putObject

  pure $ decodeUtf8 uploadUrlByteString
