module Endpoints.ExecuteForgetMeRequest (executeForgetMeRequest) where

import           Control.Concurrent      (forkIO)
import           Control.Monad           (forM_, void, when)
import           Control.Monad.Except    (MonadError (throwError))
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Reader    (MonadReader, asks)
import           Data.Profunctor         (dimap, lmap)
import           Data.String.Interpolate (i)
import qualified Data.Text               as Text
import           Data.Types.Injective    (to)
import           Data.UUID               (UUID)
import           Email                   (CommentNotificationRecipient (..),
                                          sendEmailInvitation)
import           Endpoints.GetEvent      (getEvent)
import           Hasql.Connection        (Connection)
import           Hasql.Session           (CommandError (ResultError),
                                          ResultError (ServerError),
                                          SessionError (QueryError))
import qualified Hasql.Session           as Hasql
import           Hasql.Statement         (Statement)
import           Hasql.TH                (maybeStatement, resultlessStatement,
                                          singletonStatement, vectorStatement)
import           Servant                 (ServerError (errBody), err400, err404,
                                          err500)

import qualified Email
import           Types.AppEnv            (AppEnv (..), SmtpConfig (..))
import qualified Types.Attendee          as Attendee
import           Types.Attendee          (Attendee, writeStatus)
import           Types.Event             (Event)
import           Types.ForgetMeRequest   (ExecuteForgetMeResult (..),
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
      pure $ ExecuteForgetMeResult forgetMeRequestId deletedAt
    Left err -> do
      liftIO $ putStrLn [i|Something went wrong when executing forget me request: #{err}|]
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
          Hasql.statement (email, forgetMeRequestId)
            [singletonStatement|
              with
                attendee_ids as (
                  update attendees set
                    email = null,
                    deleted_at = now()
                  where email = $1::text
                  returning id as attendee_id
                ),

                deleted_attendee_data as (
                  update attendee_data
                    set name = 'deleted user'
                  from attendee_ids
                  where attendee_data.attendee_id = attendee_ids.attendee_id
                ),

                deleted_comments as (
                  update comments set
                    comment = 'Comment deleted by user',
                    deleted_at = now()
                  from attendee_ids
                  where
                    comments.attendee_id = attendee_ids.attendee_id
                )

              update forgetme_requests
              set
                deleted_at = now(),
                email = null
              where
                id = $2::uuid
              returning deleted_at::timestamptz?
            |]

