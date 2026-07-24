{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.InitPhotoUpload (initPhotoUpload, InitPhotoUploadInput, InitPhotoUploadResult (..)) where

import qualified Amazonka                as AWS
import qualified Amazonka.S3             as AWS.S3
import           Control.Monad.Catch     (MonadCatch)
import           Control.Monad.Except    (MonadError (throwError))
import qualified Data.Aeson              as Aeson
import qualified Data.ByteString.Lazy    as LBS
import           Data.String.Interpolate (i)
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import           Data.Time               (getCurrentTime)
import           Data.Time.Clock         (UTCTime)
import           Data.UUID               (UUID)
import qualified Data.UUID.V7            as UUID
import           RIO
import           Servant                 (ServerError (errBody), err409, err500)


import qualified Op.Db                   as Db

data InitPhotoUploadInput
  = InitPhotoUploadInput
  { fileName     :: Text
  , base64Sha256 :: Text
  }
  deriving (Generic)

instance Aeson.FromJSON InitPhotoUploadInput

data InitPhotoUploadResult m
  = InitPhotoUploadResult
  { id                 :: m UUID
  , uploadUrl          :: m Text
  , materializedStatus :: Text
  , photoId            :: Maybe UUID
  }
  deriving (Generic)

instance Aeson.ToJSON (InitPhotoUploadResult Identity)
instance Aeson.ToJSON (InitPhotoUploadResult Maybe)
instance Aeson.FromJSON (InitPhotoUploadResult Identity)
instance Aeson.FromJSON (InitPhotoUploadResult Maybe)

newtype ConflictResponseBody
  = ConflictResponseBody
  { expiresAt :: UTCTime }
  deriving (Generic)

instance Aeson.FromJSON ConflictResponseBody
instance Aeson.ToJSON ConflictResponseBody

initPhotoUpload ::
  ( MonadError ServerError m
  , MonadIO m
  , MonadReader env m
  , Db.HasDbConnection env
  , HasLogFunc env
  , MonadCatch m
  ) => InitPhotoUploadInput
  -> m (InitPhotoUploadResult Maybe)
initPhotoUpload InitPhotoUploadInput{fileName, base64Sha256} = do
  photoUploadId <- UUID.genUUID
  let urlValidSeconds = 10

  presignedUploadUrl <- do
    -- TODO: move aws env to reader env
    env <- AWS.newEnv AWS.discover --  AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, and AWS_REGION env variables needed here

    let objectKey = [i|photos/#{photoUploadId}/#{fileName}|]
    let bucketName = AWS.S3.BucketName "organize-party"
    let s3PutRequestBody = AWS.toBody ("" :: ByteString)

    let putObject = AWS.S3.newPutObject bucketName objectKey s3PutRequestBody
    currentTime <- liftIO getCurrentTime
    decodeUtf8 <$> AWS.presignURL env currentTime (fromIntegral urlValidSeconds) putObject

  jsonResult <- do
    Db.queryDbOr handleErr do
      Db.statement
        (photoUploadId, fileName, presignedUploadUrl, base64Sha256, urlValidSeconds)
        [Db.singletonStatement|
          select aws.init_photo_upload(
            upload_id_                    => $1::uuid,
            file_name_                    => $2::text,
            photo_upload_url_             => $3::text,
            base64_sha256_                => $4::text,
            upload_url_lifetime_seconds_  => $5::int
          )::jsonb
        |]

  case Aeson.fromJSON jsonResult of
    Aeson.Success result -> pure result
    Aeson.Error err -> do
      logError [i|Could not parse event json: #{Aeson.encode jsonResult}\n\nerr: #{err}|]
      throwError err500 { errBody = "Something went wrong" }
  where
    handleErr err = do
      case err of
        -- conflict when the same photo is already in the process of being uploaded
        Db.StatementSessionError _ _ _ _ _ (Db.ServerStatementError (Db.ServerError "23505" msg _ _ _)) -> do
          case (Aeson.decode (LBS.fromStrict $ encodeUtf8 msg) :: Maybe ConflictResponseBody) of
            Just conflictResponse -> do
              logWarn [i|Two people are trying to upload the same file at the same time|]
              throwError err409 { errBody = Aeson.encode conflictResponse }
            _ -> do
              logError [i|Something went wrong initiating photo upload: #{err}|]
              throwError err500 { errBody = "Something went wrong" }
        _ -> do
          logError [i|Something went wrong initiating photo upload: #{err}|]
          throwError err500 { errBody = "Something went wrong" }

