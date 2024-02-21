module Endpoints.ExecuteForgetMeRequest (executeForgetMeRequest) where

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
                                         QueryError (QueryError),
                                         ResultError (ServerError))
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
import           Types.ForgetMeRequest  (ExecuteForgetMeResult (..),
                                         InitForgetMeInput (..))

executeForgetMeRequest ::
  (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) =>
  UUID ->
  m ExecuteForgetMeResult
executeForgetMeRequest forgetMeRequestId = do
  conn <- asks connection
  queryResult <- liftIO $ Hasql.run session conn
  case queryResult of
    Right Nothing -> do
      throwError err404 { errBody = "forget me request not found" }
    Right (Just deletedAt) -> do
      pure $ ExecuteForgetMeResult deletedAt
    Left err -> do
      liftIO $ print err
      throwError err500 { errBody = "Something went wrong" }
  where
    session = do
      mDeletedAt <- Hasql.statement forgetMeRequestId
          [maybeStatement|
            select
              email::text?,
              deleted_at::timestamptz?
            from forgetme_requests
            where
              id = $1::uuid
          |]

      case mDeletedAt of
        Nothing -> pure Nothing
        Just (Nothing, Just deletedAt) -> pure $ Just deletedAt
        Just (Just email, _) -> do
          Hasql.statement email
            [resultlessStatement|
              update comments
              set
                message = 'comment deleted by user'
              where
                email = $1::text
            |]

          Hasql.statement (email, forgetMeRequestId)
            [resultlessStatement|
              update commenters
              set
                name = 'deleted user',
                deleted_at = now(),
                gravatar_url = '',
                email = 'deleted-' || $2::uuid::text || '@organize.party'
              where
                email = $1::text
            |]

          Hasql.statement email
            [resultlessStatement|
              delete from attendees
              where
                email = $1::text
            |]

          -- timestamptz? hack to make it Maybe
          Hasql.statement forgetMeRequestId
            [singletonStatement|
              update forgetme_requests
              set
                deleted_at = now(),
                email = null
              where
                id = $1::uuid
              returning deleted_at::timestamptz?
            |]
