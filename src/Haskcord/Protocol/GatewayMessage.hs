{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Haskcord.Protocol.GatewayMessage
where

import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.Text as T

data GatewayMessage a = GatewayMessage {
      _gatewayMessageOp :: Int
    , _gatewayMessageData :: a
    , _gatewayMessageSequence :: Maybe Int
    , _gatewayMessageEventName :: Maybe T.Text
    }
    deriving (Show, Eq, Read)

makeLenses ''GatewayMessage

instance ToJSON a => ToJSON (GatewayMessage a) where
    toJSON message = object $ [
          "op" .= view gatewayMessageOp message
        , "d" .= view gatewayMessageData message
        ] ++ case view gatewayMessageSequence message of
                Nothing -> []
                Just seq -> ["s" .= seq ]
          ++ case view gatewayMessageEventName message of
                Nothing -> []
                Just event -> ["t" .= event]

instance FromJSON a => FromJSON (GatewayMessage a) where
    parseJSON (Object v) = GatewayMessage
                            <$> v .: "op"
                            <*> v .: "d"
                            <*> v .:? "s"
                            <*> v .:? "t"
