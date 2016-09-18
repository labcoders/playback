{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Playback.DB
( insertRecording
, getRecordingById
, getAccountByUsername
, insertTokenForAccount
, getTokenByAccountId
, insertAccount
, module Playback.DB.Monad
) where

import Playback.Api.Types
import Playback.DB.Monad

import Control.Monad.IO.Class
import Control.Concurrent.Chan
import Database.PostgreSQL.Simple
import Data.Maybe ( listToMaybe )

type MonadIODB m = (Monad m, MonadIO m, MonadDB m)

insertRecordingQ :: Query
insertRecordingQ
  = "INSERT INTO recording \
  \ ( recordedAt, recordingLength, recording, latitude, longitude ) \
  \ VALUES ( ?, ?, ?, ?, ? ) \
  \ RETURNING id"

-- | Inserts an 'AudioRecording' into the database, returning its id.
insertRecording :: MonadIODB m => InsertAudioRecording -> m Int
insertRecording recording = withConnection $ \conn -> do
  [Only recordingId] <- liftIO $ query conn insertRecordingQ recording
  pure recordingId

getRecordingByIdQ :: Query
getRecordingByIdQ
  = "SELECT recordedAt, recordingLength, recording, latitude, longitude \
  \ FROM recording \
  \ WHERE id = ?"

getRecordingById :: MonadIODB m => Int -> m (Maybe GetAudioRecording)
getRecordingById n = withConnection $ \conn ->
  listToMaybe <$> liftIO (query conn getRecordingByIdQ (Only n))

updateTokenExpiryQ :: Query
updateTokenExpiryQ
  = "UPDATE token SET expires_at = now() + interval '30 days' WHERE id = ?"

updateTokenExpiry :: MonadIODB m => TokenId -> m Bool
updateTokenExpiry tokenId = withConnection $ \conn -> do
  n <- liftIO (execute conn updateTokenExpiryQ (Only tokenId))
  pure (n /= 0)

getTokenByAccountIdQ :: Query
getTokenByAccountIdQ
  = "SELECT id, token, expires_at FROM token WHERE account_id = ?"

insertTokenForAccountQ :: Query
insertTokenForAccountQ
  = "INSERT INTO token ( token, account_id, expires_at ) \
  \ VALUES ( md5(random()::text), ?, now() + interval '30 days' ) \
  \ RETURNING id, token, expires_at"

insertTokenForAccount :: MonadIODB m => AccountId -> m TokenRecord
insertTokenForAccount account = withConnection $ \conn -> do
  [(id, token, expiresAt)] <- liftIO $
    query conn insertTokenForAccountQ (Only account)
  pure TokenRecord
    { tokenId = TokenId id
    , tokenToken = Token token
    , tokenExpiry = expiresAt
    }

-- | Gets the token associated with the given account, if any. If the retrieved
-- token is expired, then it is purged from the database, and the result is
-- 'Nothing'. Otherwise, if the token is not expired, then its lifetime is
-- extended by 30 days.
getTokenByAccountId :: MonadIODB m => AccountId -> m (Maybe TokenRecord)
getTokenByAccountId account = withConnection $ \conn -> do
  token <-
    listToMaybe <$> liftIO (query conn getTokenByAccountIdQ (Only account))
  case token of
    Nothing -> pure Nothing
    Just t -> do
      updateTokenExpiry (tokenId t)
      pure (Just t)

getAccountByUsernameQ :: Query
getAccountByUsernameQ
  = "SELECT id, username, password_hash \
  \ FROM account \
  \ WHERE username = ?"

getAccountByUsername :: MonadIODB m => Username -> m (Maybe Account)
getAccountByUsername name = withConnection $ \conn ->
  listToMaybe <$> liftIO (query conn getAccountByUsernameQ (Only name))

insertAccountQ :: Query
insertAccountQ
  = "INSERT INTO account ( username, password_hash ) \
  \ VALUES ( ?, ? ) \
  \ RETURNING id"

insertAccount :: MonadIODB m => Username -> PasswordHash -> m AccountId
insertAccount username password = withConnection $ \conn -> do
  [Only id] <- liftIO (query conn insertAccountQ (username, password))
  pure (AccountId id)
