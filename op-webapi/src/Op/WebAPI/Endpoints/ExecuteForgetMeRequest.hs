{-# LANGUAGE QuasiQuotes #-}

module Op.WebAPI.Endpoints.ExecuteForgetMeRequest (executeForgetMeRequest) where

import           Control.Monad.Except            (MonadError (throwError))
import           Data.UUID                       (UUID)
import           RIO
import           Servant                         (ServerError (errBody), err404)

import qualified Op.Db                           as Db
import           Op.WebAPI.Types.AppEnv          (AppEnv (..))
import           Op.WebAPI.Types.ForgetMeRequest (ExecuteForgetMeResult (..))

executeForgetMeRequest ::
  (MonadError ServerError m, MonadIO m, MonadReader AppEnv m) =>
  UUID ->
  m ExecuteForgetMeResult
executeForgetMeRequest forgetMeRequestId = do
  queryResult <- Db.queryDbOr Db.printAndThrow500 session
  case queryResult of
    Nothing -> do
      throwError err404 { errBody = "forget me request not found" }
    Just deletedAt -> do
      pure $ ExecuteForgetMeResult forgetMeRequestId deletedAt
  where
    session = do
      mDeletedAt <- Db.statement forgetMeRequestId
          [Db.maybeStatement|
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
        Just (Nothing, Nothing) -> error "Impossible: constraint prevents both from being null at the same time"
        Just (Just email, _) -> do
          Db.statement (email, forgetMeRequestId)
            [Db.singletonStatement|
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

