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
import qualified Network.Wreq.Session as W
import Network.WebSockets
import Wuss

import Haskcord.Gateway
import Haskcord.Token
import Haskcord.Protocol.GatewayMessage
import Haskcord.Protocol.GatewayIdentify
import Haskcord.Protocol.Hello
import Haskcord.Request

data ConnectionError = ConnectionError Status

-- | Since the Haskell websocket API only supports bracket-style
-- connecting, it is implicit that we can only support this as well.
connect :: (Client -> IO a) -> Token -> ExceptT ConnectionError IO a
connect f token = ExceptT . W.withSession $ \session -> runExceptT $ do 
    gateway <- withExceptT ConnectionError $ drop 6 . T.unpack <$> getNewGateway session token
    liftIO $ putStrLn gateway
    liftIO . runSecureClient gateway 443 "/?v=5" $ runWebsocket f gateway session token

-- | Given the client function, the gateway, and the connection,
-- run the websocket.
runWebsocket :: (Client -> IO a) -> String -> W.Session -> Token -> Connection -> IO a
runWebsocket f gateway session token conn = do
    connMVar <- newMVar conn
    sessionMVar <- newMVar session
    lastSequenceMVar <- newMVar Nothing
    f $ Client {
          _clientGateway = gateway
        , _clientToken = token
        , _clientWebsocket = connMVar
        , _clientLastSequence = lastSequenceMVar
        , _clientSession = sessionMVar
        }

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
        let (Just (Object obj)) = decode txt
        case H.lookup "op" obj of
            -- TODO handle the errors and show them.
            runExceptT $ handleEventDispatch (fromJSON $ obj H.! "t") client (obj H.! "d")
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
