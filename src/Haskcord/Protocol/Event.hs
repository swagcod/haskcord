module Haskcord.Protocol.Event
where

import Haskcord.Entity.User

data ReadyEvent = ReadyEvent {
      _readyEventProtocolVersion :: Int
    , _readyEventUser :: UserResource
    , _readyEventPrivateChannels :: [ChannelEntity]
    , 
    }
