{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}

module Op.Worker.Jobs.PollS3FileUpload (PollS3FileUploadJob) where

import qualified Data.Aeson              as Aeson
import           Data.Aeson.QQ           (aesonQQ)
import           Data.String.Interpolate (i)
import qualified Data.Text               as Text
import           Data.UUID
import           RIO

import qualified Op.Aws                  as Aws
import qualified Op.Db                   as Db
import qualified Op.Worker.Job           as Job

newtype PollS3FileUploadJob = PollS3FileUploadJob { photoUploadId :: UUID }
  deriving (Generic, Show)

instance Aeson.ToJSON PollS3FileUploadJob
instance Aeson.FromJSON PollS3FileUploadJob

instance (Db.HasDbConnection env, Aws.HasAwsEnv env) => Job.JobDefinition env PollS3FileUploadJob where
  processJob (PollS3FileUploadJob uploadId) = do
    (uploadUrl, fileName) <- do
      mUpload <- do
        Db.queryDbOr retryDbErr do
          Db.statement
            uploadId
              [Db.maybeStatement|
                select
                  upload_url::text,
                  file_name::text
                from aws.photo_uploads
                where id = $1::uuid
              |]

      case mUpload of
        Nothing     -> Job.giveUpJob [i|Couldn't find `aws.photo_uploads where id = '#{uploadId}' when polling for upload status`|]
        Just upload -> pure upload

    -- ask the S3 API if the object exists rather than HEADing the url
    -- anonymously, a private bucket responds 403 whether it exists or not
    uploaded <- Aws.objectExists (Aws.photoObjectKey uploadId fileName)

    -- the upload url has a bunch of query parameters to enable PUTing, we
    -- don't need that
    let photoUrl = Text.takeWhile (/= '?') uploadUrl

    (fsmEvent, fsmEventBody) <-
      if uploaded
        then pure ("upload_verified", Just [aesonQQ|{"photoUrl": #{photoUrl}}|])
        else pure ("poll_upload_status_again", Nothing)

    Db.queryDbOr retryDbErr do
      Db.statement
        (uploadId, fsmEvent, fsmEventBody)
        [Db.resultlessStatement|
          select fsm.notify_state_machine(
            shard => 1,
            machine => state_machine_id,
            event => $2::text,
            data => coalesce($3::jsonb?, '{}'::jsonb)
          )::text?
          from aws.photo_uploads
          where id = $1::uuid
        |]

retryDbErr :: Db.SessionError -> Job.Job env a
retryDbErr err = Job.retryJob [i|Error when accessing db for poll s3 file upload job: #{err}|]
