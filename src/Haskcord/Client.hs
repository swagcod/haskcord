{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Haskcord.Client
where

import Control.Concurrent.MVar
import Control.Lens
import qualified Data.Map.Strict as M
import Network.WebSockets
import qualified Network.Wreq.Session as W

import Haskcord.Resource.Base
import Haskcord.Resource.Id
import Haskcord.Resource.Guild
import Haskcord.Resource.GuildChannel
import Haskcord.Token


data Client = Client {
      _clientGateway :: String
    , _clientToken :: Token
    , _clientWebsocket :: MVar Connection
    , _clientLastSequence :: MVar (Maybe Int)
    , _clientSession :: MVar W.Session
    , _clientCache :: MVar Cache
    }

data Cache = Cache {
      _cacheGuilds :: M.Map GuildId GuildResource
    , _cacheGuildChannels :: M.Map GuildChannelId GuildChannelResource
    }
    deriving (Show, Read, Eq)

makeLenses ''Client
makeLenses ''Cache

class (Resource a, Ord i) => CacheResource a i | a -> i where
    cacheLens :: Lens' Cache (M.Map i a)
    resourceIdLens :: Lens' a i

instance CacheResource GuildResource GuildId where
    cacheLens = cacheGuilds
    resourceIdLens = guildResourceId

instance CacheResource GuildChannelResource GuildChannelId where
    cacheLens = cacheGuildChannels
    resourceIdLens = guildChannelResourceId

cacheResource :: CacheResource a i => Client -> a -> IO ()
cacheResource client resource = modifyMVar_ (view clientCache client) insertIntoCache
    where insertIntoCache c = return $ over cacheLens insertInto c
          insertInto resCache = M.insert (view resourceIdLens resource) resource resCache
