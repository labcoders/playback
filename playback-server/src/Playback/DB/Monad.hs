{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Playback.DB.Monad
( MonadDB(..)
, DbT()
, ConnectionPool()
, ConnectionState()
, makeConnectionPool
, runDbT
) where

import Control.Concurrent.Chan
import Control.Monad.Reader
import Control.Monad.State
import Database.PostgreSQL.Simple

-- | Class of monads that can perform arbitrary database operations.
class MonadDB m where
  withConnection :: (Connection -> m a) -> m a

-- | A concrete monad transformer for performing arbitrary database operations.
newtype DbT m a
  = DbT
    { unDbT :: ReaderT ConnectionPool (StateT ConnectionState m) a
    }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )

-- | The internal state of 'DbT'.
data ConnectionState
  = ConnectionState
    { currentConnection :: Maybe Connection
    }

-- | The static configuration of 'DbT'.
data ConnectionPool
  = ConnectionPool
    { connectionPool :: Chan Connection
    , connectionPoolSize :: Int
    }

-- | Runs a 'DbT' action, with a connection pool of a given size.
runDbT :: (Monad m, MonadIO m) => ConnectionPool -> DbT m a -> m a
runDbT pool db = do
  let initial = ConnectionState { currentConnection = Nothing }
  fst <$> runStateT (runReaderT (unDbT db) pool) initial

-- | Create a new connection pool.
--
-- See 'runDbT'.
makeConnectionPool :: ConnectInfo -> Int -> IO ConnectionPool
makeConnectionPool info n = do
  chan <- newChan
  forM_ [1..n] $ \_ -> do
    writeChan chan =<< connect info
  pure $ ConnectionPool
    { connectionPool = chan
    , connectionPoolSize = n
    }

getCurrentConnection :: Monad m => DbT m (Maybe Connection)
getCurrentConnection = DbT $ gets currentConnection

setCurrentConnection :: Monad m => Maybe Connection -> DbT m ()
setCurrentConnection conn
  = DbT $ modify $ \s -> s { currentConnection = conn }

getConnectionPool :: Monad m => DbT m (Chan Connection)
getConnectionPool = DbT $ asks connectionPool

waitForConnection :: MonadIO m => Chan Connection -> DbT m Connection
waitForConnection pool = lift $ liftIO (readChan pool)

releaseConnection :: (Monad m, MonadIO m) => Connection -> DbT m ()
releaseConnection conn = do
  pool <- getConnectionPool
  liftIO $ writeChan pool conn

instance MonadTrans DbT where
  lift = DbT . lift . lift

instance MonadIO m => MonadDB (DbT m) where
  withConnection f = do
    mconn <- getCurrentConnection
    x <- case mconn of
      Nothing -> do
        conn <- waitForConnection =<< getConnectionPool
        setCurrentConnection (Just conn)
        x <- f conn
        setCurrentConnection Nothing
        releaseConnection conn
        pure x
      Just conn -> do
        x <- f conn
        pure x
    pure x
