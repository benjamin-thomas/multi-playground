{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Concurrent (forkFinally)
import Control.Exception (try)
import Control.Monad (forever, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.List as List
import Network.Socket (
    AddrInfo (addrAddress, addrFamily),
    SockAddr,
    Socket,
    SocketOption (ReuseAddr),
    SocketType (Stream),
    accept,
    bind,
    close,
    defaultProtocol,
    getAddrInfo,
    listen,
    setSocketOption,
    socket,
 )
import Network.Socket.ByteString (recv, send)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hSetBuffering, stdout)

{-

stack run --silent -- --directory /tmp/
rg --files | entr -cr stack run --silent -- --directory /tmp/
ghcid -c 'stack repl' -T ':!doctest ./app/'

Or:

stack exec doctest ./app/

NOTE: I had to `stack install doctest` to fix incompatible versions

 -}

newtype Host = MkHost String
newtype Port = MkPort String
newtype FilesDir = MkFilesDir String

main :: IO ()
main = do
    BC.putStrLn "Booting up..."

    args <- getArgs
    filesDir <- case args of
        "--directory" : dir : _ -> pure dir
        _ -> do
            putStrLn "Missing switch: --directory PATH, using /dev/null"
            pure "/dev/null"

    putStrLn $ "filesDir: " <> show filesDir

    hSetBuffering stdout LineBuffering

    let host = "127.0.0.1"
        port = "4221"

    addrInfos <- getAddrInfo Nothing (Just host) (Just port)
    case addrInfos of
        addrInfo : _ -> boot addrInfo (MkHost host) (MkPort port) (MkFilesDir filesDir)
        _ -> do
            BC.putStrLn "Failed to resolve address (or too many addresses)"
            exitFailure

    BC.putStrLn "Shutting down..."

{- |

>>> findFirstHeaderValue (BC.pack "User-Agent") $ map BC.pack ["X-Hello: 1", "User-Agent: foo", "User-Agent: bar"]
Just "foo"
-}
findFirstHeaderValue :: BC.ByteString -> [BC.ByteString] -> Maybe BC.ByteString
findFirstHeaderValue key headers = do
    kv <- List.find (BC.isPrefixOf key) headers
    case BC.split ':' kv of
        [_k, v] -> Just $ BC.strip v
        _ -> Nothing

userAgent :: Socket -> [BC.ByteString] -> IO ()
userAgent clientSocket headers = do
    case findFirstHeaderValue "User-Agent" headers of
        Nothing -> do
            void $ send clientSocket "HTTP/1.1 400 Bad Request\r\n"
        Just val -> do
            void $ send clientSocket "HTTP/1.1 200 OK\r\n"
            void $ send clientSocket "Content-Type: text/plain\r\n"
            void $ send clientSocket $ "Content-Length: " <> BC.pack (show $ BC.length val) <> "\r\n"
            void $ send clientSocket "\r\n"
            void $ send clientSocket val

{- |

>>> BC.pack "hello" </> BC.pack "world"
"hello/world"
>>> BC.pack "hello/" </> BC.pack "/world"
"hello/world"
>>> BC.pack "hello/" </> BC.pack "world"
"hello/world"
>>> BC.pack "hello" </> BC.pack "/world"
"hello/world"
-}
(</>) :: BC.ByteString -> BC.ByteString -> BC.ByteString
(</>) a b = case (BC.isSuffixOf "/" a, BC.isPrefixOf "/" b) of
    (False, False) -> a <> "/" <> b
    (True, True) -> a <> BC.drop 1 b
    _ -> a <> b

newtype Headers = MkHeaders [BC.ByteString] deriving (Show)
newtype Path = MkPath BC.ByteString
newtype Body = MkBody [BC.ByteString] deriving (Show)

handlePost :: Socket -> Headers -> Body -> Path -> FilesDir -> IO ()
handlePost clientSocket (MkHeaders _) (MkBody body) (MkPath path) (MkFilesDir filesDir) = do
    case drop 1 $ BC.split '/' path of
        ["files", filename] -> do
            filePath <- BS.toFilePath $ BC.pack filesDir </> filename
            let body_ = BS.concat body
            try (BS.writeFile filePath body_) >>= \case
                Left (_ :: IOError) -> do
                    putStrLn "==> Failed to write file!"
                    void $ send clientSocket "HTTP/1.1 500 Internal Server Error\r\n"
                    void $ send clientSocket "\r\n"
                Right _ -> do
                    putStrLn "==> Successfully wrote file!"
                    void $ send clientSocket "HTTP/1.1 201 Created\r\n"
                    void $ send clientSocket "Content-Type: application/octet-stream\r\n"
                    void $ send clientSocket "\r\n"
        _ -> do
            BC.putStrLn $ "==> Unhandled path: " <> path
            void $ send clientSocket "HTTP/1.1 404 Not Found\r\n"

handleGet :: Socket -> Headers -> Path -> FilesDir -> IO ()
handleGet clientSocket (MkHeaders headers) (MkPath path) (MkFilesDir filesDir) = do
    case drop 1 $ BC.split '/' path of
        [""] -> do
            void $ send clientSocket "HTTP/1.1 200 OK\r\n\r\n"
        ["echo", content] -> do
            -- http 127.0.0.1:4221/echo/hello
            let contentLength = BC.pack $ show $ BC.length content
            void $ send clientSocket "HTTP/1.1 200 OK\r\n"
            void $ send clientSocket "Content-Type: text/plain\r\n"
            void $ send clientSocket $ "Content-Length: " <> contentLength <> "\r\n"
            void $ send clientSocket "\r\n"
            void $ send clientSocket content
        "user-agent" : _ ->
            -- http 127.0.0.1:4221/user-agent User-Agent:foo
            userAgent clientSocket headers
        ["files", filename] -> do
            filePath <- BS.toFilePath $ BC.pack filesDir </> filename
            putStrLn "==> Before read"
            try (BC.readFile filePath) >>= \case
                Left (_ :: IOError) -> do
                    putStrLn "==> After read (fail)"
                    void $ send clientSocket "HTTP/1.1 404 Not Found\r\n"
                    void $ send clientSocket "\r\n"
                Right fileContent -> do
                    putStrLn "==> After read (success)"
                    void $ send clientSocket "HTTP/1.1 200 OK\r\n"
                    void $ send clientSocket "Content-Type: application/octet-stream\r\n"
                    void $ send clientSocket $ "Content-Length: " <> BC.pack (show $ BC.length fileContent) <> "\r\n"
                    void $ send clientSocket "\r\n"
                    void $ send clientSocket fileContent
        _ -> do
            BC.putStrLn $ "==> Unhandled path: " <> path
            void $ send clientSocket "HTTP/1.1 404 Not Found\r\n"
            void $ send clientSocket "\r\n"

-- A record type
data Request = MkRequest
    { requestLine :: BC.ByteString
    , requestHeaders :: Headers
    , requestBody :: Body
    }
    deriving (Show)

toLines :: BC.ByteString -> [BC.ByteString]
toLines = map (BC.filter (/= '\r')) . BC.lines

_testString :: BC.ByteString
_testString =
    BC.concat
        $ map
            (<> "\r\n")
            [ "GET /user-agent HTTP/1.1"
            , "Accept: */*"
            , "Accept-Encoding: gzip, deflate"
            , "Connection: keep-alive"
            , "Host: 127.0.0.1:4221"
            , "User-Agent: foobar/1.2.3"
            , "X-WAT: 1\r\n"
            , "This is the body"
            ]

{- |

>>> toLines _testString !! 0
"GET /user-agent HTTP/1.1"

>>> toLines _testString !! 1
"Accept: */*"

>>> toRequest _testString
Just (MkRequest {requestLine = "GET /user-agent HTTP/1.1", requestHeaders = MkHeaders ["Accept: */*","Accept-Encoding: gzip, deflate","Connection: keep-alive","Host: 127.0.0.1:4221","User-Agent: foobar/1.2.3","X-WAT: 1"], requestBody = MkBody ["This is the body"]})
-}
toRequest :: BC.ByteString -> Maybe Request
toRequest str =
    case BC.breakSubstring "\r\n" str of
        (_, "") -> Nothing
        (firstLine, rest) ->
            let (headers, body) = BC.breakSubstring "\r\n\r\n" $ BC.drop 2 rest
             in Just
                    $ MkRequest
                        { requestLine = firstLine
                        , requestHeaders = MkHeaders $ toLines headers
                        , requestBody = MkBody $ toLines $ BC.drop 4 body
                        }

handleConn :: Socket -> SockAddr -> FilesDir -> IO ()
handleConn clientSocket clientAddr filesDir = do
    BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."

    req <- recv clientSocket 1024
    case toRequest req of
        Nothing -> do
            BC.putStrLn "Bad client"
            void $ send clientSocket "HTTP/1.1 500 Internal Server Error\r\n\r\n"
        Just MkRequest{requestLine, requestHeaders, requestBody} -> do
            case BC.words requestLine of
                ["GET", path, "HTTP/1.1"] -> do
                    handleGet clientSocket requestHeaders (MkPath path) filesDir
                ["POST", path, "HTTP/1.1"] -> do
                    handlePost clientSocket requestHeaders requestBody (MkPath path) filesDir
                unhandled -> do
                    BC.putStrLn $ "==> Unhandled request: " <> BC.pack (show unhandled)
                    void $ send clientSocket "HTTP/1.1 500 Internal Server Error\r\n\r\n"

{-

time for x in $(seq 1 999);do echo -n "$x: " && curl http://127.0.0.1:4221/user-agent&&echo;done
=> 5.9s

 -}
boot :: AddrInfo -> Host -> Port -> FilesDir -> IO ()
boot addrInfo (MkHost host) (MkPort port) filesDir = do
    serverSocket <- socket (addrFamily addrInfo) Stream defaultProtocol
    setSocketOption serverSocket ReuseAddr 1
    bind serverSocket $ addrAddress addrInfo
    listen serverSocket 5

    BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port
    forever $ do
        BC.putStrLn "Waiting for connections..."
        (clientSocket, clientAddr) <- accept serverSocket
        forkFinally
            (handleConn clientSocket clientAddr filesDir)
            (\_ -> close clientSocket >> BC.putStrLn "Closed connection")
