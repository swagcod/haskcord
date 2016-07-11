{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Haskcord.Gateway
where

import Control.Applicative
import Control.Monad.Except
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

import Haskcord.Request
import Haskcord.Token
import Haskcord.Routes (routeGateway)

data GatewayResponse = GatewayResponse {
    _gatewayUrl :: T.Text
    }
    deriving (Show, Read, Eq)

makeLenses ''GatewayResponse

instance FromJSON GatewayResponse where
    parseJSON (Object o) = GatewayResponse <$> o .: "url"
    parseJSON _ = empty

getNewGateway :: Session -> Token -> ExceptT Status IO T.Text
getNewGateway session token = view gatewayUrl <$> get session routeGateway token
