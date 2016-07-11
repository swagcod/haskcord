{-# LANGUAGE FunctionalDependencies #-}
module Haskcord.Client
where

import Haskcord.Resource.Id
import Haskcord.Resource.Guild

data Client = Client {
      _clientGateway :: String
    , _clientToken :: Token
    , _clientWebsocket :: MVar Connection
    , _clientLastSequence :: MVar (Maybe Int)
    , _clientCache :: MVar Cache
    }

data Cache = Cache {
      _cacheGuilds :: H.HashMap GuildId GuildResource
    }

makeLenses ''Client
makeLenses ''Cache

class Resource a => CacheResource a i | a -> i where
    cacheLens :: Lens Cache (H.HashMap i a)
