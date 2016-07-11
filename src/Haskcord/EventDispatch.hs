module Haskcord.EventDispatch
where

import qualified Data.Text as T

import Haskcord.Client

data EventError = ParseError String

handleEventDispatch :: T.Text -> Client -> Value -> ExceptT EventError IO ()
handleEventDispatch "EVENT_GUILD_CREATE" = handleGuildCreate
handleEventDispatch _ = return ()

handleGuildCreate :: Client -> Value -> ExceptT EventError IO ()
handleGuildCreate client json = do
    guildResource <- withExceptT . ExceptT . fromJSON $ json
    cache guildResource
