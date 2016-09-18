{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Playback.Api.Types where

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.Scientific ( toRealFloat )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField hiding ( Binary )
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import GHC.Generics

data Account
  = Account
    { accountId :: AccountId
    , accountUsername :: Username
    , accountPasswordHash :: PasswordHash
    }

newtype AccountId
  = AccountId
    { unAccountId :: Int
    }

newtype Username
  = Username
    { unUsername :: T.Text
    }

newtype Password
  = Password
    { unPassword :: T.Text
    }

newtype PasswordHash
  = PasswordHash
    { unPasswordHash :: B.ByteString
    }

data AuthRequest
  = AuthRequest
    { authUsername :: Username
    , authPassword :: Password
    }

data AuthResult
  = AuthResultOk
    { authToken :: Token
    }
  | AuthResultFailed
    { authMessage :: T.Text
    }

newtype Token
  = Token
    { unToken :: B.ByteString
    }

data TokenRecord
  = TokenRecord
    { tokenId :: TokenId
    , tokenToken :: Token
    , tokenExpiry :: UTCTime
    }

newtype TokenId
  = TokenId
    { unTokenId :: Int
    }

-- | Datatype representing an audio recording.
data AudioRecording
    = AudioRecording
    { recording :: B.ByteString
    , timestamp :: UTCTime
    , location :: Maybe GeographicalPosition
    , duration :: Duration
    }

-- | Datatype representing a position on Earth.
data GeographicalPosition
  = GeographicalPosition
    { latitude :: Double
    , longitude :: Double
    }

newtype InsertAudioRecording
  = InsertAudioRecording
    { unInsertAudioRecording :: AudioRecording
    }

newtype GetAudioRecording
  = GetAudioRecording
    { unGetAudioRecording :: AudioRecording
    }

newtype Duration
  = Duration
    { unDuration :: Double
    }
  deriving ( Eq, Ord, Read, Show )

data Ping
  = Ping
    { ping :: UTCTime
    }
  deriving ( Eq, FromJSON, Generic, Ord, Read, Show, ToJSON )

data Pong
  = Pong
    { pong :: UTCTime
    }
  deriving ( Eq, FromJSON, Generic, Ord, Read, Show, ToJSON )

newtype B64 = B64 { unB64 :: B.ByteString }

instance ToField TokenId where
  toField (TokenId n) = toField n

instance ToField AccountId where
  toField (AccountId n) = toField n

instance ToField Username where
  toField (Username username) = toField username

instance FromRow TokenRecord where
  fromRow = TokenRecord
    <$> (TokenId <$> field)
    <*> (Token <$> field)
    <*> field

instance FromRow Account where
  fromRow = Account
    <$> (AccountId <$> field)
    <*> (Username <$> field)
    <*> (PasswordHash <$> field)

instance FromJSON AuthRequest where
  parseJSON v = case v of
    Object o -> AuthRequest
      <$> o .: "username"
      <*> o .: "password"
    _ -> fail "cannot parse AuthRequest from non-object"

instance FromJSON Username where
  parseJSON (String s) = pure (Username s)

instance FromJSON Password where
  parseJSON (String s) = pure (Password s)

instance ToField PasswordHash where
  toField (PasswordHash hash) = toField hash

instance ToJSON AuthResult where
  toJSON v = case v of
    AuthResultOk (Token token) ->
      object [ "success" .= True, "token" .= B64 token ]
    AuthResultFailed msg -> object [ "success" .= False, "message" .= msg ]

instance ToRow InsertAudioRecording where
  toRow (InsertAudioRecording rec)
    = toRow
      ( timestamp rec
      , unDuration (duration rec)
      , Binary (recording rec)
      , latitude <$> location rec
      , longitude <$> location rec
      )

instance FromRow GetAudioRecording where
  fromRow = do
    (f1, f2, f3, f4, f5) <- (,,,,)
      <$> field <*> field <*> field <*> field <*> field
    pure $ GetAudioRecording
      AudioRecording
        { timestamp = f1
        , duration = Duration f2
        , recording = f3
        , location = GeographicalPosition <$> f4 <*> f5
        }

instance FromJSON Duration where
  parseJSON (Number n) = pure $ Duration (toRealFloat n)

instance ToJSON Duration where
  toJSON (Duration n) = toJSON n

instance FromJSON GeographicalPosition where
  parseJSON v = case v of
    Object o -> GeographicalPosition
      <$> o .: "latitude"
      <*> o .: "longitude"
    _ -> fail "Failed to decode GeographicalPosition from non-object."

instance FromJSON AudioRecording where
  parseJSON v = case v of
    Object o -> AudioRecording
      <$> (unB64 <$> o .: "recording")
      <*> o .: "timestamp"
      <*> o .: "location"
      <*> o .: "duration"

instance ToJSON GeographicalPosition where
  toJSON r = object
    [ "latitude" .= latitude r
    , "longitude" .= longitude r
    ]

instance ToJSON AudioRecording where
  toJSON r = object
    [ "recording" .= B64 (recording r)
    , "timestamp" .= timestamp r
    , "location" .= location r
    , "duration" .= duration r
    ]

instance FromJSON B64 where
  parseJSON v = case v of
    String t -> either fail (pure . B64) . B64.decode $ TE.encodeUtf8 t
    _ -> fail "Cannot decode base64 to ByteString from non-text."

instance ToJSON B64 where
  toJSON = String . TE.decodeUtf8 . B64.encode . unB64
