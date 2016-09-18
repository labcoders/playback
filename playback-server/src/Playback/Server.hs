{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Playback.Server
( webMain
) where

import Playback.Api
import Playback.Crypto
import Playback.DB
import Playback.Server.Monad

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Resource
import Data.Conduit.Audio
import Data.Conduit.Audio.LAME
import Data.Conduit.Audio.Sndfile
import Data.String.Conversions ( cs, (<>) )
import qualified Data.ByteString as B
import Database.PostgreSQL.Simple ( defaultConnectInfo, ConnectInfo(..) )
import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.Gunzip
import Network.Wai.Middleware.RequestLogger
import Servant
import System.IO
import System.IO.Temp

-- | Get handler that returns the 'AudioRecording' represented
-- by the provided item ID.
getUpload :: Int -> Playback AudioRecording
getUpload item = getRecordingById item >>= \case
  Nothing -> throwError (NoSuchRecording item)
  Just (GetAudioRecording recording) -> pure recording

getAudio :: Int -> Playback B.ByteString
getAudio n = undefined

-- | Post handler that puts the provided 'AudioRecording' and
-- returns the id of the item on the server.
postUpload :: AudioRecording -> Playback Int
postUpload rec = do
  audio <- liftIO $ withSystemTempFile "rec.temp.pcm" $
    \fp h -> do
      B.hPut h $ recording rec
      audSource <- sourceSnd fp
      runResourceT $ sinkMP3 fp audSource
      hSeek h AbsoluteSeek 0
      B.hGetContents h

  insertRecording (InsertAudioRecording
      rec { recording = audio })

postPing :: Ping -> Playback Pong
postPing (Ping p) = pure (Pong p)

postAuth :: AuthRequest -> Playback AuthResult
postAuth auth = withTransaction $ do
  let username = authUsername auth
  let password = authPassword auth

  maccount <- getAccountByUsername username

  case maccount of
    Nothing -> do
      hash <- liftIO $ makePassword password
      accountId <- insertAccount username hash
      token <- insertTokenForAccount accountId
      pure AuthResultOk
        { authToken = tokenToken token}

    Just account -> do
      let hashedPassword = accountPasswordHash account
      if verifyPassword password hashedPassword
      then do
        getTokenByAccountId (accountId account) >>= \case
          Nothing -> pure AuthResultFailed
            { authMessage = "something went wrong" }
          Just token -> pure AuthResultOk
            { authToken = tokenToken token }
      else do
        pure AuthResultFailed
          { authMessage = "Username or password is incorrect." }

server :: ConnectionPool -> Server PlaybackAPI
server pool = enter playback endpoints where
  endpoints :: ServerT PlaybackAPI Playback
  endpoints
    = getUpload
    :<|> getAudio
    :<|> postUpload
    :<|> postPing
    :<|> postAuth

  playback :: PlaybackT IO :~> Handler
  playback = Nat $ \a ->
    liftIO (runPlaybackT pool a) >>= \case
      Right x -> pure x
      Left e -> case e of
        DatabaseError sqlError -> throwError err500
          { errBody = "database error" }
        UnexpectedException exc -> throwError err500
          { errBody = "unexpected exception" }
        NoSuchRecording n -> throwError err404
          { errBody = "no such recording " <> cs (show n) }

app :: ConnectionPool -> Application
app pool = serve api (server pool)

defaultInfo :: ConnectInfo
defaultInfo = defaultConnectInfo
  { connectUser = "playback"
  , connectDatabase = "playback"
  }

defaultPoolSize :: Int
defaultPoolSize = 10

middleware = gunzip . logStdoutDev

webMain :: IO ()
webMain = do
  pool <- makeConnectionPool defaultInfo defaultPoolSize
  run 8082 $ middleware $ (app pool)
