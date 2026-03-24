{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.InitForgetMeRequest (initForgetMe) where

import           Control.Monad.Except            (MonadError)
import           Data.String.Interpolate         (__i)
import qualified Data.UUID                       as UUID
import           RIO
import           Servant                         (ServerError)

import qualified Op.Db                           as Db
import           Op.WebAPI.Types.AppEnv          (AppEnv (..))
import           Op.WebAPI.Types.ForgetMeRequest (InitForgetMeInput (..),
                                                  InitForgetMeResult (..))

initForgetMe ::
  (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) =>
  InitForgetMeInput ->
  m InitForgetMeResult
initForgetMe InitForgetMeInput{email} = do
  forgetMeRequestId <- Db.queryDbOr Db.printAndThrow500 (Db.statement email statement)

  hostUrl' <- asks hostUrl
  sendForgetMeConfirmation Db.printAndThrow500 hostUrl' forgetMeRequestId email

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

        select id::uuid
        from inserted

        union

        select id::uuid
        from forgetme_requests
        where email = $1::text
     |]


sendForgetMeConfirmation ::
  (Db.HasDbConnection env, MonadIO m, MonadReader env m) =>
  (Db.SessionError -> m ()) ->
  String ->
  UUID.UUID ->
  Text ->
  m ()
sendForgetMeConfirmation onError hostUrl forgetMeRequestId email = do
  let subject = "Forget me request"
  Db.queryDbOr onError (Db.statement (email, subject, body) statement)
  where
    body =
      [__i|
        A request to delete your data has been received. If you did not make
        this request, please ignore this email.
        <br>
        <br>
        If you did make this request, please click the link below to confirm. <b>Warning: this will delete all your data, it cannot be undone</b>
        <br>
        <a href="#{hostUrl}/forget-me/#{forgetMeRequestId}">#{hostUrl}/forget-me/#{forgetMeRequestId}</a>
        <br>
        <br>
        It will not delete events created by you, there's no connection between email addresses and events. It's impossible to tell which ones were created by you.
      |]
    statement =
      [Db.resultlessStatement|
        insert into email.emails (
          recipient_email,
          subject,
          body
        )
        values (
          $1::text,
          $2::text,
          $3::text
        )
      |]
