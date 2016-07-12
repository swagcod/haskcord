{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskcord.Resource.Guild
where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as V
import qualified Data.Text as T

import Haskcord.Resource.Base
import Haskcord.Resource.Id

-- | This is incomplete.
--
-- Documented fields TODO: 
--   voice_states
--   emojis
--   features
--   roles
data GuildResource = GuildResource {
      _guildResourceId :: GuildId
    , _guildName :: T.Text
    , _guildIconHash :: T.Text
    , _guildSplashHash :: Maybe T.Text
    , _guildOwnerId :: UserId
    , _guildRegion :: T.Text
    , _guildAfkChannelId :: GuildChannelId
    , _guildAfkTimeout :: Int
    , _guildVerificationLevel :: Int -- TODO better type
    , _guildChannelIds :: V.Vector GuildChannelId
    }
    deriving (Show, Read, Eq)

makeLenses ''GuildResource

instance FromJSON GuildResource where
    parseJSON (Object v) = GuildResource
        <$> v .: "id"
        <*> v .: "name"
        <*> v .: "icon"
        <*> v .: "splash"
        <*> v .: "owner_id"
        <*> v .: "region"
        <*> v .: "afk_channel_id"
        <*> v .: "afk_timeout"
        <*> v .: "verification_level"
        <*> (v .: "channels" >>= parseChannels)
    parseJSON _ = mempty

instance Resource GuildResource where
    syncResource = error "unimplemented lol"

parseChannels :: Value -> Parser (V.Vector GuildChannelId)
parseChannels (Array a) = mapM parseChannel a
    where parseChannel (Object v) = v .: "id"
          parseChannel _ = mempty
parseChannels _ = mempty
