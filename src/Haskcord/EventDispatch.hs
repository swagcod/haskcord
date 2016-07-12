{-# LANGUAGE OverloadedStrings #-}
module Haskcord.EventDispatch
where

import Control.Monad.Except
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Vector as V

import Haskcord.Client
import Haskcord.Resource.Guild
import Haskcord.Resource.GuildChannel

data EventError = ParseError String
    deriving (Show, Read, Eq)

parseResource :: FromJSON a => Value -> ExceptT EventError IO a
parseResource = withExceptT ParseError . ExceptT . return . convertResult . fromJSON

convertResult :: Result a -> Either String a
convertResult (Success a) = Right a
convertResult (Error e) = Left e

withResult :: Result a -> ExceptT EventError IO a
withResult = withExceptT ParseError . ExceptT . return . convertResult

handleEventDispatch :: T.Text -> Client -> Value -> ExceptT EventError IO ()
handleEventDispatch "GUILD_CREATE" = handleGuildCreate
handleEventDispatch _ = const . const $ return ()

parseInnerChannels :: Value -> Parser (V.Vector Value)
parseInnerChannels = withObject "guild object" $ \obj -> do 
    channels <- obj .: "channels"
    withArray "channels vector" return channels

handleGuildCreate :: Client -> Value -> ExceptT EventError IO ()
handleGuildCreate client json = do
    guildResource <- parseResource json :: ExceptT EventError IO GuildResource
    liftIO . putStrLn $ "caching" ++ show guildResource
    liftIO . cacheResource client $ guildResource
    channels <- withResult $ parse parseInnerChannels json
    forM_ channels $ \channel -> do
        liftIO $ print channel
        channelResource <- parseResource channel :: ExceptT EventError IO GuildChannelResource
        liftIO . putStrLn $ "caching" ++ show channelResource
        liftIO . cacheResource client $ channelResource