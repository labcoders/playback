module Playback.Crypto where

import qualified Crypto.PasswordStore as PW
import Data.ByteString ( ByteString )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Playback.Api.Types

strength :: Int
strength = 17

makePassword :: Password -> IO PasswordHash
makePassword (Password password)
  = PasswordHash
  <$> PW.makePassword (TE.encodeUtf8 password) strength

verifyPassword :: Password -> PasswordHash -> Bool
verifyPassword (Password password) (PasswordHash hash)
  = PW.verifyPassword (TE.encodeUtf8 password) hash
