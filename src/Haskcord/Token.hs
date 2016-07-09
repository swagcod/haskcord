{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Haskcord.Token
where

import Network.Wreq
import Network.HTTP.Types.Header
import Control.Lens
import qualified Data.ByteString as B

newtype Token = Token { _mkToken :: B.ByteString }
makeLenses ''Token

authorizationOptions :: Token -> Options
authorizationOptions (Token token) = agent & header hAuthorization .~ [token]
    where agent = defaults & header hUserAgent .~ ["DiscordBot (https://github.com/swagcod, 0.0.0)" :: B.ByteString]
