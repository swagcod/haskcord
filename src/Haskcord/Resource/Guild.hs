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
    , _guildResourceName :: T.Text
    , _guildResourceIconHash :: T.Text
    , _guildResourceSplashHash :: Maybe T.Text
    , _guildResourceOwnerId :: UserId
    , _guildResourceRegion :: T.Text
    , _guildResourceAfkChannelId :: GuildChannelId
    , _guildResourceAfkTimeout :: Int
    , _guildResourceVerificationLevel :: Int -- TODO better type
    , _guildResourceChannelIds :: V.Vector GuildChannelId
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
