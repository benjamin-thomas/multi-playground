{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Haskell SSE server using raw TCP sockets for immediate disconnection detection
-- This bypasses WAI/Warp buffering limitations

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, handle)
import Control.Monad (forM, forM_, forever)
import Data.Aeson (ToJSON (..), encode, object, (.=))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics (Generic)
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (recv, send)
import System.IO (hFlush, hSetEncoding, stdout, utf8)

port :: Int
port = 5005

-- SSE Client data structure
data SSEClient = SSEClient
    { clientId :: String
    , clientConnectedAt :: UTCTime
    , clientActive :: TVar Bool
    , clientLastPing :: TVar UTCTime
    }
    deriving (Eq)

type ClientMap = TVar (Map.Map String SSEClient)

-- JSON message types
data SSEMessage = SSEMessage
    { msgType :: String
    , msgClientId :: String
    , msgMessage :: String
    , msgTimestamp :: Int
    , msgTotalClients :: Int
    }
    deriving (Generic, Show)

instance ToJSON SSEMessage where
    toJSON msg =
        object
            [ "type" .= msgType msg
            , "clientId" .= msgClientId msg
            , "message" .= msgMessage msg
            , "timestamp" .= msgTimestamp msg
            , "totalClients" .= msgTotalClients msg
            ]

-- Generate alphabetic IDs (A, B, C, ..., Z, AA, AB, ...)
generateAlphabeticId :: Int -> String
generateAlphabeticId n = go n ""
  where
    go num acc
        | num < 0 = acc
        | otherwise =
            let remainder = num `mod` 26
                quotient = num `div` 26
                char = [toEnum (65 + remainder)]
             in if quotient == 0
                    then char ++ acc
                    else go (quotient - 1) (char ++ acc)

-- Log message with timestamp
logMessage :: String -> IO ()
logMessage msg = do
    now <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.000Z" now
    putStrLn $ "[" ++ timestamp ++ "] " ++ msg
    hFlush stdout

-- Cleanup client
cleanupClient :: String -> ClientMap -> IO ()
cleanupClient clientIdStr clientsRef = do
    clients <- atomically $ do
        clients <- readTVar clientsRef
        case Map.lookup clientIdStr clients of
            Just client -> do
                writeTVar (clientActive client) False
                let newClients = Map.delete clientIdStr clients
                writeTVar clientsRef newClients
                return newClients
            Nothing -> return clients

    logMessage $ "Client " ++ clientIdStr ++ " removed. Remaining: " ++ show (Map.size clients)

-- Parse simple HTTP request
parseHttpRequest :: BS.ByteString -> Maybe (String, String)
parseHttpRequest request = do
    let requestLines = BS.lines request
    case requestLines of
        (requestLine : _) -> do
            let parts = BS.words requestLine
            case parts of
                [method, path, _version] ->
                    Just (BS.unpack method, BS.unpack path)
                _ -> Nothing
        _ -> Nothing

-- Send HTTP response headers
sendHttpHeaders :: Socket -> String -> [(String, String)] -> IO ()
sendHttpHeaders sock contentType additionalHeaders = do
    let headers =
            [ "HTTP/1.1 200 OK\r\n"
            , "Content-Type: " ++ contentType ++ "\r\n"
            , "Cache-Control: no-cache\r\n"
            , "Connection: keep-alive\r\n"
            , "Access-Control-Allow-Origin: *\r\n"
            ]
                ++ map (\(name, value) -> name ++ ": " ++ value ++ "\r\n") additionalHeaders
                ++ ["\r\n"]
    let response = BS.pack $ concat headers
    _ <- send sock response
    return ()

-- Send SSE event
sendSSEEvent :: Socket -> String -> String -> String -> Int -> Int -> IO Bool
sendSSEEvent sock eventType clientIdStr message timestamp totalClients = do
    let sseMsg = SSEMessage eventType clientIdStr message timestamp totalClients
    let jsonData = "data: " ++ LBS.unpack (encode sseMsg) ++ "\n\n"
    handle (\(_ :: SomeException) -> return False) $ do
        _ <- send sock (BS.pack jsonData)
        return True

-- Handle SSE events endpoint
handleEventsEndpoint :: Socket -> ClientMap -> TVar Int -> IO ()
handleEventsEndpoint sock clientsRef counterRef = do
    -- Generate client ID
    counter <- atomically $ do
        counter <- readTVar counterRef
        writeTVar counterRef (counter + 1)
        return counter
    let clientIdStr = generateAlphabeticId counter

    logMessage $ "Client " ++ clientIdStr ++ " connecting..."

    -- Create client record
    connectedAt <- getCurrentTime
    activeVar <- newTVarIO True
    lastPingVar <- newTVarIO connectedAt
    let client = SSEClient clientIdStr connectedAt activeVar lastPingVar

    -- Add client to map
    atomically $ do
        clients <- readTVar clientsRef
        writeTVar clientsRef (Map.insert clientIdStr client clients)

    clientCount <- atomically $ do
        clients <- readTVar clientsRef
        return $ Map.size clients

    logMessage $ "Client " ++ clientIdStr ++ " connected. Total clients: " ++ show clientCount

    -- Send SSE headers
    sendHttpHeaders sock "text/event-stream" []

    -- Send initial connected message
    timestamp <- floor . (* 1000) . utcTimeToPOSIXSeconds <$> getCurrentTime
    success <- sendSSEEvent sock "connected" clientIdStr "Haskell SSE connection established" timestamp clientCount

    if success
        then do
            -- Start ping loop - this will detect disconnections immediately via socket exceptions
            let pingLoop = do
                    active <- readTVarIO activeVar
                    if not active
                        then
                            cleanupClient clientIdStr clientsRef
                        else do
                            -- Wait 0.5 seconds
                            threadDelay 500000

                            active' <- readTVarIO activeVar
                            if active'
                                then do
                                    now <- getCurrentTime
                                    let timestamp' = floor $ (* 1000) $ utcTimeToPOSIXSeconds now

                                    clientCount' <- atomically $ do
                                        clients <- readTVar clientsRef
                                        return $ Map.size clients

                                    -- Send heartbeat comment first
                                    heartbeatSuccess <- handle (\(_ :: SomeException) -> return False) $ do
                                        _ <- send sock ": heartbeat\n"
                                        return True

                                    if heartbeatSuccess
                                        then do
                                            -- Send ping event - direct socket write will fail immediately on disconnection
                                            pingSuccess <- sendSSEEvent sock "ping" clientIdStr "ping from Haskell" timestamp' clientCount'

                                            if pingSuccess
                                                then do
                                                    atomically $ writeTVar lastPingVar now
                                                    logMessage $ "Ping sent to " ++ clientIdStr
                                                    pingLoop
                                                else do
                                                    logMessage $ "Client " ++ clientIdStr ++ " disconnected (ping failed)"
                                                    cleanupClient clientIdStr clientsRef
                                        else do
                                            logMessage $ "Client " ++ clientIdStr ++ " disconnected (heartbeat failed)"
                                            cleanupClient clientIdStr clientsRef
                                else cleanupClient clientIdStr clientsRef

            pingLoop
        else do
            logMessage $ "Client " ++ clientIdStr ++ " disconnected (initial message failed)"
            cleanupClient clientIdStr clientsRef

-- Handle status endpoint
handleStatusEndpoint :: Socket -> ClientMap -> IO ()
handleStatusEndpoint sock clientsRef = do
    clients <- readTVarIO clientsRef

    let clientList = Map.toList clients
    clientsData <- forM clientList $ \(cid, c) -> do
        let connectedAtMs = floor $ (* 1000) $ utcTimeToPOSIXSeconds (clientConnectedAt c)
        return $
            object
                [ "id" .= cid
                , "connectedAt" .= (connectedAtMs :: Int)
                ]

    let statusData =
            object
                [ "activeConnections" .= Map.size clients
                , "clients" .= clientsData
                , "implementation" .= ("Haskell/Raw TCP (immediate disconnection detection)" :: String)
                ]

    sendHttpHeaders sock "application/json" []
    _ <- send sock (LBS.toStrict $ encode statusData)
    return ()

-- Handle OPTIONS endpoint
handleOptionsEndpoint :: Socket -> IO ()
handleOptionsEndpoint sock = do
    sendHttpHeaders
        sock
        "text/plain"
        [ ("Access-Control-Allow-Methods", "GET, OPTIONS")
        , ("Access-Control-Allow-Headers", "Content-Type")
        ]

-- Handle 404
handle404 :: Socket -> IO ()
handle404 sock = do
    let response =
            BS.pack $
                "HTTP/1.1 404 Not Found\r\n"
                    ++ "Content-Type: text/plain\r\n"
                    ++ "Access-Control-Allow-Origin: *\r\n"
                    ++ "\r\n"
                    ++ "Not Found\n\nAvailable endpoints:\n- /events (SSE)\n- /status (JSON)"
    _ <- send sock response
    return ()

-- Handle client connection
handleClient :: Socket -> ClientMap -> TVar Int -> IO ()
handleClient sock clientsRef counterRef = do
    result <- handle
        ( \(e :: SomeException) -> do
            logMessage $ "Connection error: " ++ show e
            return Nothing
        )
        $ do
            -- Read HTTP request
            requestData <- recv sock 4096
            case parseHttpRequest requestData of
                Just ("GET", "/events") -> do
                    handleEventsEndpoint sock clientsRef counterRef
                    return $ Just ()
                Just ("GET", "/status") -> do
                    handleStatusEndpoint sock clientsRef
                    return $ Just ()
                Just ("OPTIONS", _) -> do
                    handleOptionsEndpoint sock
                    return $ Just ()
                _ -> do
                    handle404 sock
                    return $ Just ()

    case result of
        Just _ -> return ()
        Nothing -> return ()

-- Background cleanup task
cleanupStaleClients :: ClientMap -> IO ()
cleanupStaleClients clientsRef = forever $ do
    threadDelay 2000000 -- 2 seconds
    now <- getCurrentTime

    staleClients <- atomically $ do
        clients <- readTVar clientsRef
        Map.foldrWithKey
            ( \cid client acc -> do
                lastPing <- readTVar (clientLastPing client)
                rest <- acc
                if diffUTCTime now lastPing > 3.0 -- 3 seconds without ping = stale
                    then return (cid : rest)
                    else return rest
            )
            (return [])
            clients

    forM_ staleClients $ \clientId -> do
        logMessage $ "Detected stale client " ++ clientId ++ " - cleaning up"
        cleanupClient clientId clientsRef

-- Main server
runServer :: IO ()
runServer = do
    -- Set UTF-8 encoding for stdout to handle emojis
    hSetEncoding stdout utf8
    putStrLn ""
    putStrLn ("🟣 Haskell Raw TCP SSE Server Starting (Port " ++ show port ++ ")")
    putStrLn "✅ This implementation provides immediate disconnection detection:"
    putStrLn "   - Raw TCP sockets with Network.Socket"
    putStrLn "   - Manual HTTP parsing"
    putStrLn "   - Direct socket writes fail immediately on disconnection"
    putStrLn "   - STM for thread-safe state management"
    putStrLn ""
    putStrLn "Expected behavior: IMMEDIATE disconnection detection"
    putStrLn ""
    putStrLn "Endpoints:"
    putStrLn ("  - http://localhost:" ++ show port ++ "/events (SSE stream)")
    putStrLn ("  - http://localhost:" ++ show port ++ "/status (Active connections)")
    putStrLn ""
    hFlush stdout

    -- Initialize state
    clientsRef <- newTVarIO Map.empty
    counterRef <- newTVarIO 0

    -- Start background cleanup task
    _ <- async $ cleanupStaleClients clientsRef

    -- Create socket and bind
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet (fromIntegral port) 0)
    listen sock 10

    logMessage "Raw TCP server started successfully"

    -- Accept connections
    forever $ do
        (clientSock, _addr) <- accept sock
        _ <- async $ do
            handleClient clientSock clientsRef counterRef
            close clientSock
        return ()

main :: IO ()
main = runServer