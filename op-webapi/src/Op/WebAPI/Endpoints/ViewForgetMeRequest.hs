{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.ViewForgetMeRequest (viewForgetMeRequest) where

import           Control.Monad.Except            (MonadError (throwError))
import           Data.UUID                       (UUID)
import           RIO
import           Servant                         (ServerError (errBody), err404)

import qualified Op.Db                           as Db
import           Op.WebAPI.Types.AppEnv          (AppEnv (..))
import           Op.WebAPI.Types.ForgetMeRequest (ForgetMeRequest (..))

viewForgetMeRequest ::
  (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) =>
  UUID ->
  m ForgetMeRequest
viewForgetMeRequest forgetMeRequestId = do
  queryResult <- Db.queryDbOr Db.printAndThrow500 (Db.statement forgetMeRequestId statement)
  case queryResult of
    Nothing -> throwError err404 { errBody = "Forget me request not found" }
    Just (forgetMeId, mEmail, deletedAt) -> do
      pure $ ForgetMeRequest
        { forgetMeRequestId = forgetMeId
        , forgetMeRequestEmail = mEmail
        , forgetMeRequestDeletedAt = deletedAt
        }
  where
    statement =
      [Db.maybeStatement|
        select
          id::uuid,
          email::text?,
          deleted_at::timestamptz?
        from forgetme_requests
        where id = $1::uuid
      |]
