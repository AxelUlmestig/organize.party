{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.ExecuteForgetMeRequest (executeForgetMeRequest) where

import           Control.Monad.Except            (MonadError (throwError))
import           Data.UUID                       (UUID)
import           RIO
import           Servant                         (ServerError (errBody), err404)

import qualified Op.Db                           as Db
import           Op.WebAPI.Types.ForgetMeRequest (ExecuteForgetMeResult (..))

executeForgetMeRequest ::
  ( MonadError ServerError m
  , MonadIO m
  , MonadReader env m
  , Db.HasDbConnection env
  ) =>
  UUID ->
  m ExecuteForgetMeResult
executeForgetMeRequest forgetMeRequestId = do
  queryResult <- do
    Db.queryDbOr Db.printAndThrow500 do
      Db.statement
        forgetMeRequestId
        [Db.singletonStatement|
        select forget_email_address($1::uuid)::timestamptz?
        |]

  case queryResult of
    Nothing -> do
      throwError err404 { errBody = "forget me request not found" }
    Just deletedAt -> do
      pure $ ExecuteForgetMeResult forgetMeRequestId deletedAt

