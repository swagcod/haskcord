{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Haskcord.Protocol.Hello
where

import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.Text as T

import Haskcord.Protocol.GatewayMessage
import Haskcord.Token

data HelloMessage = HelloMessage {
    _heartbeatInterval :: Int
    }

makeLenses ''HelloMessage

instance FromJSON HelloMessage where
    parseJSON (Object v) = HelloMessage
        <$> v .: "heartbeat_interval"
