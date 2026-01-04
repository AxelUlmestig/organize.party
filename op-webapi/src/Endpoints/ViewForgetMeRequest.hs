module Endpoints.ViewForgetMeRequest (viewForgetMeRequest) where

import           Control.Concurrent     (forkIO)
import           Control.Monad          (forM_, void, when)
import           Control.Monad.Except   (MonadError (throwError))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Profunctor        (dimap, lmap)
import qualified Data.Text              as Text
import           Data.Types.Injective   (to)
import           Data.UUID              (UUID)
import           Endpoints.GetEvent     (getEvent)
import           Hasql.Connection       (Connection)
import           Hasql.Session          (CommandError (ResultError),
                                         ResultError (ServerError),
                                         SessionError (QueryError))
import qualified Hasql.Session          as Hasql
import           Hasql.Statement        (Statement)
import           Hasql.TH               (maybeStatement, resultlessStatement,
                                         singletonStatement, vectorStatement)
import qualified Op.Db                  as Db
import           Servant                (ServerError (errBody), err400, err404,
                                         err500)

import qualified Email
import           Types.AppEnv           (AppEnv (..), SmtpConfig (..))
import           Types.ForgetMeRequest  (ForgetMeRequest (..))

viewForgetMeRequest ::
  (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) =>
  UUID ->
  m ForgetMeRequest
viewForgetMeRequest forgetMeRequestId = do
  queryResult <- Db.queryDbOr Db.printAndThrow500 (Hasql.statement forgetMeRequestId statement)
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
      [maybeStatement|
        select
          id::uuid,
          email::text?,
          deleted_at::timestamptz?
        from forgetme_requests
        where id = $1::uuid
      |]
