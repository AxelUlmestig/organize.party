module Op.WebAPI.Types.HasHostUrl (
  HasHostUrl(..)
) where

import RIO

class HasHostUrl a where
  getHostUrl :: a -> Text
