{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Haskcord.Protocol.GatewayIdentify
where

import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Haskcord.Protocol.GatewayMessage
import Haskcord.Token

data GatewayIdentifyProperties = GatewayIdentifyProperties {
      _os :: T.Text
    , _browser :: T.Text
    , _device :: T.Text
    , _referrer :: T.Text
    , _referringDomain :: T.Text
    }

makeLenses ''GatewayIdentifyProperties

data GatewayIdentifyMessage = GatewayIdentifyMessage {
      _gatewayToken :: Token
    , _properties :: GatewayIdentifyProperties
    , _compress :: Bool
    , _largeThreshold :: Int
    }

makeLenses ''GatewayIdentifyMessage

instance ToJSON GatewayIdentifyProperties where
    toJSON properties = object [
          "$os" .= view os properties
        , "$browser" .= view browser properties
        , "$device" .= view device properties
        , "$referrer" .= view referrer properties
        , "$referring_domain" .= view referringDomain properties
        ]

instance ToJSON GatewayIdentifyMessage where
    toJSON message = object [
          "token" .= (TE.decodeUtf8 . view mkToken . view gatewayToken $ message)
        , "properties" .= view properties message
        , "compress" .= view compress message
        , "large_threshold" .= view largeThreshold message
        ]

defaultGatewayIdentifyProperties = GatewayIdentifyProperties {
      _os = "Linux"
    , _browser = "DiscordBot Hasckord"
    , _device = "DiscordBot Haskcord"
    , _referrer = ""
    , _referringDomain = ""
    }

defaultGatewayIdentifyMessage :: Token -> GatewayIdentifyMessage
defaultGatewayIdentifyMessage tok = GatewayIdentifyMessage {
      _gatewayToken = tok
    , _properties = defaultGatewayIdentifyProperties
    , _compress = False
    , _largeThreshold = 250
    }

makeGatewayIdentifyMessage :: GatewayIdentifyMessage -> GatewayMessage GatewayIdentifyMessage
makeGatewayIdentifyMessage message = GatewayMessage {
      _gatewayMessageOp = 2
    , _gatewayMessageData = message
    , _gatewayMessageSequence = Nothing
    , _gatewayMessageEventName = Nothing
    }
