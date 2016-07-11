module Haskcord.Resource.Channel
where

import qualified Data.Text as T

import Haskcord.Resource.Id

data GuildChannelResource = GuildChannelResource {
      _guildChannelResourceId :: Guild.ChannelId
    , _guildChannelGuildId :: GuildId
    , _guildChannelName :: T.Text
    , _guildChannelType :: T.Text
    , _guildChannelPosition :: Int
    , _guildChannelPermissionOverwrites :: [OverwriteResource]
    , _guildChannelTopic :: Maybe T.Text
    , _guildChannelLastMessageId :: Maybe MessageId
    , _guildChannelBitrate :: Maybe Int
    , _guildChannelUserLimit :: Maybe Int
    }
    deriving (Show, Eq, Read)

instance FromJSON GuildChannelResource where

