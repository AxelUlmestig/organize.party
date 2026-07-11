{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.InitPhotoUpload (initPhotoUpload, InitPhotoUploadInput, InitPhotoUploadResult) where

import qualified Amazonka                    as AWS
import qualified Amazonka.S3                 as AWS.S3
import           Control.Monad.Catch         (MonadCatch)
import           Control.Monad.Except        (MonadError (throwError))
import qualified Data.Aeson                  as Aeson
import           Data.String.Interpolate     (i)
import qualified Data.Text                   as Text
import           Data.Text.Encoding          (decodeUtf8)
import           Data.Time                   (getCurrentTime)
import           Data.UUID                   (UUID)
import qualified Data.UUID.V7                as UUID
import           RIO
import           Servant                     (ServerError (errBody), err400,
                                              err404, err500)


import qualified Op.Db                       as Db
import           Op.WebAPI.Types.PhotoUpload (PhotoUpload (..))

newtype InitPhotoUploadInput
  = InitPhotoUploadInput { fileName :: Text }
  deriving (Generic)
instance Aeson.FromJSON InitPhotoUploadInput

data InitPhotoUploadResult
  = InitPhotoUploadResult
  { id                 :: UUID
  , uploadUrl          :: Text
  , materializedStatus :: Text
  , photoId            :: Maybe Text
  }
  deriving (Generic)

instance Aeson.ToJSON InitPhotoUploadResult

initPhotoUpload ::
  ( MonadError ServerError m
  , MonadIO m
  , MonadReader env m
  , Db.HasDbConnection env
  , HasLogFunc env
  , MonadCatch m
  ) => InitPhotoUploadInput
  -> m InitPhotoUploadResult
initPhotoUpload InitPhotoUploadInput{fileName} = do
  photoUploadId <- UUID.genUUID

  presignedUploadUrl <- do
    -- TODO: move aws env to reader env
    env <- AWS.newEnv AWS.discover --  AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, and AWS_REGION env variables needed here

    let objectKey = [i|photos/#{photoUploadId}/#{fileName}|]
    let bucketName = AWS.S3.BucketName "organize-party"
    let s3PutRequestBody = AWS.toBody ("" :: ByteString)

    let putObject = AWS.S3.newPutObject bucketName objectKey s3PutRequestBody
    let urlValidSeconds = 10
    currentTime <- liftIO getCurrentTime
    decodeUtf8 <$> AWS.presignURL env currentTime urlValidSeconds putObject

  materializedStatus <- do
    Db.queryDbOr handleErr do
      Db.statement
        (photoUploadId, fileName, presignedUploadUrl)
        [Db.singletonStatement|
          insert into aws.photo_uploads (id, file_name, upload_url)
          values ($1::uuid, $2::text, $3::text)
          returning materialized_status::text
        |]

  pure $ InitPhotoUploadResult { id = photoUploadId, uploadUrl = presignedUploadUrl, materializedStatus, photoId = Nothing }

  where
    handleErr err = do
      logError [i|Something went wrong when attending event: #{err}|]
      throwError err500 { errBody = "Something went wrong" }

