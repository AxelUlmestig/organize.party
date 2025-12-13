module Endpoints.InitForgetMeRequest (initForgetMe) where

import           Control.Concurrent     (forkIO)
import           Control.Monad          (forM_, void, when)
import           Control.Monad.Except   (MonadError (throwError))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Profunctor        (dimap, lmap)
import qualified Data.Text              as Text
import           Data.Types.Injective   (to)
import           Data.UUID              (UUID)
import           Email                  (CommentNotificationRecipient (..),
                                         sendEmailInvitation)
import           Endpoints.GetEvent     (getEvent)
import           Hasql.Connection       (Connection)
import           Hasql.Session          (CommandError (ResultError),
                                         ResultError (ServerError),
                                         SessionError (QueryError))
import qualified Hasql.Session          as Hasql
import           Hasql.Statement        (Statement)
import           Hasql.TH               (maybeStatement, resultlessStatement,
                                         singletonStatement, vectorStatement)
import           Servant                (ServerError (errBody), err400, err404,
                                         err500)

import qualified Email
import           Types.AppEnv           (AppEnv (..), SmtpConfig (..))
import qualified Types.Attendee         as Attendee
import           Types.Attendee         (Attendee, writeStatus)
import           Types.Event            (Event)
import           Types.ForgetMeRequest  (InitForgetMeInput (..),
                                         InitForgetMeResult (..))
import qualified Util.Db                as Db

initForgetMe ::
  (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) =>
  InitForgetMeInput ->
  m InitForgetMeResult
initForgetMe InitForgetMeInput{email} = do
  (forgetMeRequestId, email) <- Db.queryDbOr Db.printAndThrow500 (Hasql.statement email statement)

  smtpConf <- asks smtpConfig
  hostUrl' <- asks hostUrl
  void . liftIO . forkIO $ Email.sendForgetMeConfirmation hostUrl' smtpConf forgetMeRequestId email

  pure $ InitForgetMeResult
    { initForgetMeResultEmail = email
    }
  where
    statement =
      [singletonStatement|
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
