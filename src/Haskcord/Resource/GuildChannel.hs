{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Haskcord.Resource.GuildChannel
where

import Control.Lens
import Data.Aeson
import qualified Data.Text as T

import Haskcord.Resource.Base
import Haskcord.Resource.Id
import Haskcord.Permissions

-- TODO we need to "thread in" guild_id somehow.
data GuildChannelResource = GuildChannelResource {
      _guildChannelResourceId :: GuildChannelId
    --, _guildChannelResourceGuildId :: GuildId
    , _guildChannelResourceName :: T.Text
    , _guildChannelResourceType :: T.Text
    , _guildChannelResourcePosition :: Int
    , _guildChannelResourcePermissionOverwrites :: [Overwrite]
    , _guildChannelResourceTopic :: Maybe T.Text
    , _guildChannelResourceLastMessageId :: Maybe MessageId
    , _guildChannelResourceBitrate :: Maybe Int
    , _guildChannelResourceUserLimit :: Maybe Int
    }
    deriving (Show, Eq, Read)

makeLenses ''GuildChannelResource

instance FromJSON GuildChannelResource where
    parseJSON = withObject "guildchannel" $ \v -> GuildChannelResource
        <$> v .: "id"
        -- <*> v .: "guild_id"
        <*> v .: "name"
        <*> v .: "type"
        <*> v .: "position"
        <*> v .: "permission_overwrites"
        <*> v .:? "topic"
        <*> v .:? "last_message_id"
        <*> v .:? "bitrate"
        <*> v .:? "user_limit"

instance Resource GuildChannelResource where
    syncResource = error "unimplemented lol"