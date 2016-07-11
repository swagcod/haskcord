{-# LANGUAGE TemplateHaskell #-}
module Haskcord.Resource.Guild
where

import Haskcord.Resource.Id

import qualified Data.Text as T

data GuildResource = GuildResource {
      _guildResourceId :: GuildId
    , _guildName :: T.Text
    , _guildIconHash :: T.Text
    , _guildSplashHash :: T.Text
    , _guildOwnerId :: UserId
    , _guild
    }
