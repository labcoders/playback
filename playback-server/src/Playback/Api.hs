{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Playback.Api
( type PlaybackAPI
, api
, module Playback.Api.Types
) where

import Playback.Api.Types

import Data.ByteString ( ByteString )
import Servant

-- | Two endpoints:
--
-- GET /uploads/:item
--  - Header contains metadata of item
--      - Time of recording
--      - GPS coordinates
--  - Returns base 64 encoded JSON blob
--    which is the recording.
--
-- POST /uploads/:item
--  - Header contains metadata of item
--      - Time of recording
--      - GPS recording
type PlaybackAPI
    = "uploads"
      :> Capture "itemID" Int
      :> Get '[JSON] AudioRecording
    :<|> "uploads"
      :> Capture "itemID" Int
      :> "audio"
      :> Get '[OctetStream] ByteString
    :<|> "uploads"
      :> ReqBody '[JSON] AudioRecording
      :> Post '[JSON] Int
    :<|> "ping"
      :> ReqBody '[JSON] Ping
      :> Post '[JSON] Pong
    :<|> "auth"
      :> ReqBody '[JSON] AuthRequest
      :> Post '[JSON] AuthResult

-- | Boilerplate.
api :: Proxy PlaybackAPI
api = Proxy
