{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Haskcord.Client
where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TE
import Network.WebSockets
import Wuss

import Haskcord.Gateway
import Haskcord.Token
import Haskcord.Protocol.GatewayMessage
import Haskcord.Protocol.GatewayIdentify
import Haskcord.Protocol.Hello
import Haskcord.Request

-- | Client for a persistent connection to Discord. Consists of the token
-- used for connecting, the websocket, and the gateway that it is connected
-- to (which can be used for resuming).
--
-- TODO investigate storing a Wreq session here.
data Client = Client {
      _clientGateway :: String
    , _clientToken :: Token
    , _clientWebsocket :: MVar Connection
    , _clientLastSequence :: MVar (Maybe Int)
    }

data ConnectionError = ConnectionError Status

makeLenses ''Client

-- | Since the Haskell websocket API only supports bracket-style
-- connecting, it is implicit that we can only support this as well.
connect :: (Client -> IO a) -> Token -> ExceptT ConnectionError IO a
connect f token = do 
    gateway <- withExceptT ConnectionError $ drop 6 . T.unpack <$> getNewGateway token
    liftIO $ putStrLn gateway
    liftIO . runSecureClient gateway 443 "/?v=5" $ runWebsocket f gateway token

-- websocketHeaders :: Token -> Headers
-- websocketHeaders (Token token) = [("Authorization", token), ("User-Agent", "DiscordBot (https://github.com/swagcod, 0.0.0)")]

-- | Given the client function, the gateway, and the connection,
-- run the websocket.
runWebsocket :: (Client -> IO a) -> String -> Token -> Connection -> IO a
runWebsocket f gateway token conn = do
    connMVar <- newMVar conn
    lastSequenceMVar <- newMVar Nothing
    let client = Client {
            _clientGateway = gateway
          , _clientToken = token
          , _clientWebsocket = connMVar
          , _clientLastSequence = lastSequenceMVar
          }
    f client

connectionTest :: Token -> ExceptT ConnectionError IO a
connectionTest = connect $ \client -> do
    websocket <- readMVar $ view clientWebsocket client
    helloTxt <- receiveData websocket
    TL.putStrLn . TE.decodeUtf8 $ helloTxt

    sendTextData2 websocket . encode . toJSON . makeGatewayIdentifyMessage $ defaultGatewayIdentifyMessage (view clientToken client)

    let Just (Object v) = decode helloTxt
    if fromJSON (v H.! "op") == Success (10 :: Int)
        then do putStrLn "got hello"
                let (Just helloObject) = decode helloTxt
                let (Success helloMessage) = fromJSON helloObject :: Result (GatewayMessage HelloMessage)
                let interval = view heartbeatInterval $ view gatewayMessageData helloMessage
                forkIO $ runHeartbeatThread client interval
        else error "wtf"
    forever $ do
        txt <- receiveData websocket
        updateSeq client txt
        print txt

updateSeq :: Client -> BL.ByteString -> IO ()
updateSeq client txt = do
    let (Just (Object obj)) = decode txt
    case H.lookup "s" obj of
        Nothing -> return ()
        Just seq -> do
            let (Success seq') = fromJSON seq
            swapMVar (view clientLastSequence client) seq'
            return ()

sendTextData2 socket text = do
    print text
    sendTextData socket text

makeHeartbeat :: Maybe Int -> GatewayMessage (Maybe Int)
makeHeartbeat sequence = GatewayMessage {
      _gatewayMessageOp = 1
    , _gatewayMessageData = sequence
    , _gatewayMessageSequence = Nothing
    , _gatewayMessageEventName = Nothing
    }

-- | Run the heartbeat loop for a given client with the given interval
-- (which is specified in milliseconds)
runHeartbeatThread :: Client -> Int -> IO ()
runHeartbeatThread client interval = forever $ do
    conn <- readMVar $ view clientWebsocket client
    withMVar (view clientLastSequence client) $ \seq -> do
        sendTextData2 conn . encode . toJSON . makeHeartbeat $ seq
    threadDelay $ interval * 1000
