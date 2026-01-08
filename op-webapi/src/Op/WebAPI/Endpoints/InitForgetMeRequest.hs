{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.InitForgetMeRequest (initForgetMe) where

import           Control.Monad.Except            (MonadError)
import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Reader            (MonadReader, asks)
import qualified Data.UUID                       as UUID
import qualified Hasql.Session                   as Hasql
import           Servant                         (ServerError)

import qualified Op.Db                           as Db
import qualified Op.WebAPI.Email                 as Email
import           Op.WebAPI.Types.AppEnv          (AppEnv (..))
import           Op.WebAPI.Types.ForgetMeRequest (InitForgetMeInput (..),
                                                  InitForgetMeResult (..))

initForgetMe ::
  (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) =>
  InitForgetMeInput ->
  m InitForgetMeResult
initForgetMe InitForgetMeInput{email} = do
  (forgetMeRequestId, email) <- Db.queryDbOr Db.printAndThrow500 (Hasql.statement email statement)

  hostUrl' <- asks hostUrl
  Email.sendForgetMeConfirmation Db.printAndThrow500 hostUrl' forgetMeRequestId email

  pure $ InitForgetMeResult
    { initForgetMeResultEmail = email
    , initForgetMeResultId = UUID.nil -- placeholder, this gets turned into null in the ToJSON instance
    }
  where
    statement =
      [Db.singletonStatement|
        with inserted as (
          insert into forgetme_requests (email)
          values ($1::text)
          on conflict (email)
          do nothing
          returning
            id,
            email
        )

        select id::uuid, email::text
        from inserted

        union

        select id::uuid, email::text
        from forgetme_requests
        where email = $1::text
     |]
