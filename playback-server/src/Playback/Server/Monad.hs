{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Playback.Server.Monad where

import Playback.DB.Monad

import Control.Exception
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.PostgreSQL.Simple hiding ( withTransaction )

newtype PlaybackT m a
  = PlaybackT
    { unPlayback :: ExceptT PlaybackError (DbT m) a
    }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError PlaybackError
    )

instance MonadIO m => MonadDB (PlaybackT m) where
  withConnection :: (Connection -> PlaybackT m a) -> PlaybackT m a
  withConnection f = e >>= \case
    Left exc -> throwError exc
    Right x -> pure x
    where
      e = PlaybackT $ lift (withConnection f')
      f' = runExceptT . unPlayback . f

type Playback = PlaybackT IO

runPlaybackT
  :: (Monad m, MonadIO m)
  => ConnectionPool
  -> PlaybackT m a
  -> m (Either PlaybackError a)
runPlaybackT pool (PlaybackT m) = runDbT pool (runExceptT m)

data PlaybackError
  = DatabaseError SqlError
  | UnexpectedException SomeException
  | NoSuchRecording Int

-- | Runs an @IO@ action that can throw 'SqlError' and promotes it to a
-- 'PlaybackError'.
trySql :: MonadIO m => IO a -> PlaybackT m a
trySql m = do
  v <- liftIO (try m)
  case v of
    Left e -> case fromException e of
      Just sqlError -> throwError (DatabaseError sqlError)
      Nothing -> throwError (UnexpectedException $ toException e)
    Right x -> pure x

withTransaction :: Playback a -> Playback a
withTransaction m = withConnection $ \conn -> do
  liftIO $ begin conn
  x <- (pure <$> m)
    `catchError` (\e -> liftIO (rollback conn) *> pure (Left e))
  x' <- case x of
    Left e -> throwError e
    Right x -> pure x
  liftIO $ commit conn
  pure x'
