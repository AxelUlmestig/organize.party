{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.GetPhotoUpload (getPhotoUpload) where

import           Control.Monad.Catch                 (MonadCatch)
import           Control.Monad.Except                (MonadError (throwError))
import qualified Data.Aeson                          as Aeson
import           Data.String.Interpolate             (i)
import qualified Data.Text                           as Text
import           Data.Text.Encoding                  (decodeUtf8)
import           Data.Time                           (getCurrentTime)
import           Data.UUID                           (UUID)
import qualified Data.UUID.V7                        as UUID
import           RIO
import           Servant                             (ServerError (errBody),
                                                      err400, err404, err500)

import qualified Op.Db                               as Db
import           Op.WebAPI.Endpoints.InitPhotoUpload (InitPhotoUploadResult (..))

getPhotoUpload ::
  ( MonadError ServerError m
  , MonadIO m
  , MonadReader env m
  , Db.HasDbConnection env
  , HasLogFunc env
  , MonadCatch m
  ) => UUID
  -> m InitPhotoUploadResult
getPhotoUpload uploadId = do
    mUploadStatus <- do
      Db.queryDbOr handleErr do
        Db.statement
          uploadId
          [Db.maybeStatement|
            select
              id::uuid,
              upload_url::text,
              materialized_status::text,
              photo_id::uuid?
            from aws.photo_uploads
            where id = $1::uuid
          |]

    case mUploadStatus of
      Just (id, uploadUrl, materializedStatus, photoId) -> pure InitPhotoUploadResult{..}
      Nothing -> do throwError err404 { errBody = [i|Photo upload with id #{uploadId} couldn't be found|] }
  where
    handleErr err = do
      logError [i|Something went wrong getting photo upload status: #{err}|]
      throwError err500 { errBody = "Something went wrong" }



