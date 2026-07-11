module Op.WebAPI.Types.PhotoUpload (PhotoUpload(..), PhotoUploadStatus (..)) where

import qualified Data.Aeson as Aeson
import           Data.UUID  (UUID)
import           RIO

data PhotoUpload
  = PhotoUpload
    { id           :: UUID
    , uploadUrl    :: Text
    , photoId      :: Maybe UUID
    , uploadStatus :: PhotoUploadStatus
    }
    deriving (Generic, Eq, Show)

instance Aeson.ToJSON PhotoUpload
instance Aeson.FromJSON PhotoUpload

data PhotoUploadStatus
  = Initializing
  deriving (Generic, Eq, Show)

instance Aeson.ToJSON PhotoUploadStatus
instance Aeson.FromJSON PhotoUploadStatus

