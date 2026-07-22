{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}

module Op.Worker.Jobs.PollS3FileUpload (PollS3FileUploadJob) where

import qualified Data.Aeson              as Aeson
import           Data.Aeson.QQ           (aesonQQ)
import           Data.String.Interpolate (i)
import qualified Data.Text               as Text
import           Data.UUID
import qualified Network.HTTP.Req        as Req
import           RIO
import           Text.URI                (mkURI)

import qualified Op.Db                   as Db
import qualified Op.Worker.Job           as Job

newtype PollS3FileUploadJob = PollS3FileUploadJob { photoUploadId :: UUID }
  deriving (Generic, Show)

instance Aeson.ToJSON PollS3FileUploadJob
instance Aeson.FromJSON PollS3FileUploadJob

instance (Db.HasDbConnection env) => Job.JobDefinition env PollS3FileUploadJob where
  processJob (PollS3FileUploadJob uploadId) = do
    textUrl <- do
      mUrl <- do
        Db.queryDbOr retryDbErr do
          Db.statement
            uploadId
              [Db.maybeStatement|
                select upload_url::text
                from aws.photo_uploads
                where id = $1::uuid
              |]

      case mUrl of
        Nothing  -> Job.giveUpJob [i|Couldn't find `aws.photo_uploads where id = '#{uploadId}' when polling for upload status`|]
        Just url -> do
          -- the upload url has a bunch of query parameters to enable PUTing, we don't need that
          case Text.splitOn "?" url of
            (withoutQueryParams : _) -> pure withoutQueryParams
            wat -> Job.giveUpJob [i|Couldn't strip away query parameters from URL: #{url}, got: #{wat}|]

    response <- do
      (s3Url, options) <- do
        case Req.useHttpsURI =<< mkURI textUrl of
          Just uri -> pure uri
          Nothing  -> Job.giveUpJob [i|Couldn't parse AWS S3 url when polling for upload status: #{textUrl}|]

      liftIO do
        Req.runReq Req.defaultHttpConfig do
          Req.req
            Req.HEAD
            s3Url
            Req.NoReqBody
            Req.ignoreResponse
            options

    (fsmEvent, fsmEventBody) <- do
      case Req.responseStatusCode response of
        200 -> pure ("upload_verified", Just [aesonQQ|{"photoUrl": #{textUrl}}|])
        404 -> pure ("poll_upload_status_again", Nothing)
        _ -> Job.retryJob [i|Unexpected HTTP response code when calling AWS SNS subscribe url: #{Req.responseStatusCode response}, trying again...|]

    Db.queryDbOr retryDbErr do
      Db.statement
        (uploadId, fsmEvent, fsmEventBody)
        [Db.resultlessStatement|
          select fsm.notify_state_machine(
            shard => 1,
            machine => state_machine_id,
            event => $2::text,
            data => $3::jsonb?
          )::text?
          from aws.photo_uploads
          where id = $1::uuid
        |]

retryDbErr :: Db.SessionError -> Job.Job env a
retryDbErr err = Job.retryJob [i|Error when accessing db for poll s3 file upload job: #{err}|]
