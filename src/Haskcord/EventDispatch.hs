{-# LANGUAGE OverloadedStrings #-}
module Haskcord.EventDispatch
where

import Control.Monad.Except
import Data.Aeson
import qualified Data.Text as T

import Haskcord.Client
import Haskcord.Resource.Guild

data EventError = ParseError String
    deriving (Show, Read, Eq)

convertResult :: Result a -> Either String a
convertResult (Success a) = Right a
convertResult (Error e) = Left e

handleEventDispatch :: T.Text -> Client -> Value -> ExceptT EventError IO ()
handleEventDispatch "GUILD_CREATE" = handleGuildCreate
handleEventDispatch _ = const . const $ return ()

handleGuildCreate :: Client -> Value -> ExceptT EventError IO ()
handleGuildCreate client json = do
    guildResource <- withExceptT ParseError . ExceptT . return . convertResult . fromJSON $ json
    liftIO . putStrLn $ "caching" ++ show guildResource
    liftIO . cacheResource client $ (guildResource :: GuildResource)