module Server
  ( Client
  , IntervalId
  , Request
  , Response
  , Server
  , ServerState
  , clearIntervalJS
  , closeServerJS
  , createServerJS
  , endResponseJS
  , generateAlphabeticId
  , getRequestMethodJS
  , getRequestUrlJS
  , handleRequestPS
  , isResponseClosedJS
  , jsonStringifyJS
  , listenJS
  , onRequestJS
  , onResponseJS
  , parseUrlJS
  , setIntervalJS
  , setupSSEConnectionPS
  , startServer
  , tryEndResponseJS
  , writeHeadJS
  , writeResponseJS
  , writeSSE
  ) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.DateTime.Instant (Instant, unInstant)
import Effect.Now (now)
import Foreign (Foreign, unsafeToForeign)
import Data.String as String
import Data.Enum (toEnum)

-- Foreign imports for HTTP server
foreign import data Server :: Type
foreign import data Request :: Type
foreign import data Response :: Type

foreign import createServerJS :: (Request -> Response -> Effect Unit) -> Effect Server
foreign import listenJS :: Int -> Server -> Effect Unit -> Effect Unit
foreign import closeServerJS :: Server -> Effect Unit -> Effect Unit

-- Request parsing functions
foreign import getRequestUrlJS :: Request -> Effect String
foreign import getRequestMethodJS :: Request -> Effect String
foreign import parseUrlJS :: String -> Effect { pathname :: String, query :: Foreign }

-- HTTP response functions
foreign import writeHeadJS :: Response -> Int -> Foreign -> Effect Unit
foreign import endResponseJS :: Response -> String -> Effect Unit
foreign import writeResponseJS :: Response -> String -> Effect Unit
foreign import jsonStringifyJS :: Foreign -> String

-- PureScript helper for SSE format
writeSSE :: Response -> String -> Effect Unit
writeSSE res data_ = writeResponseJS res ("data: " <> data_ <> "\n\n")

-- Unified request handler in PureScript
handleRequestPS :: Ref.Ref ServerState -> Request -> Response -> Effect Unit
handleRequestPS stateRef req res = do
  -- Get request info
  url <- getRequestUrlJS req
  method <- getRequestMethodJS req
  parsedUrl <- parseUrlJS url

  -- Log request
  currentTime <- now
  let Milliseconds ms = unInstant currentTime
  log $ "[" <> show ms <> "] " <> method <> " " <> url

  -- Route based on pathname
  case parsedUrl.pathname of
    "/events" -> handleSSE stateRef req res
    "/status" -> handleStatus stateRef req res
    "/hello" -> handleHello stateRef req res
    _ -> handle404 res
  where
  handle404 res' = do
    let
      headers = unsafeToForeign
        { "Content-Type": "text/plain" }
    writeHeadJS res' 404 headers
    endResponseJS res' "Not Found\n\nAvailable endpoints:\n- /events (SSE)\n- /status (JSON)\n- /hello (JSON)"

  handleHello _ _ res' = do
    let
      headers = unsafeToForeign
        { "Content-Type": "application/json"
        , "Access-Control-Allow-Origin": "*"
        }
    writeHeadJS res' 200 headers
    let
      response = unsafeToForeign
        { message: "Hello from PureScript!"
        , implementation: "PureScript FFI"
        , timestamp: 0 -- We'll add proper timestamp later
        }
    endResponseJS res' (jsonStringifyJS response)

  handleStatus stateRef' _ res' = do
    let
      headers = unsafeToForeign
        { "Content-Type": "application/json"
        , "Access-Control-Allow-Origin": "*"
        }
    writeHeadJS res' 200 headers
    -- Get current state and build status response
    state <- Ref.read stateRef'
    let clients = Map.values state.clients
    let
      clientsJson = map
        ( \c ->
            let
              Milliseconds clientMs = unInstant c.connectedAt
            in
              { id: c.id
              , connectedAt: clientMs
              }
        )
        clients
    let
      status = unsafeToForeign
        { activeConnections: Map.size state.clients
        , clients: clientsJson
        , implementation: "PureScript/FFI (Node.js HTTP direct)"
        }
    endResponseJS res' (jsonStringifyJS status)

  handleSSE stateRef' req' res' = do
    -- Generate client ID
    state <- Ref.read stateRef'
    let clientId = generateAlphabeticId state.counter
    Ref.modify_ (\s -> s { counter = s.counter + 1 }) stateRef'

    log $ "[" <> "PureScript" <> "] Client " <> clientId <> " connecting..."

    -- Set SSE headers
    let
      headers = unsafeToForeign
        { "Content-Type": "text/event-stream"
        , "Cache-Control": "no-cache"
        , "Connection": "keep-alive"
        , "Access-Control-Allow-Origin": "*"
        }
    writeHeadJS res' 200 headers

    -- Register client
    timestamp <- now
    let client = { id: clientId, connectedAt: timestamp }
    newState <- Ref.modify (\s -> s { clients = Map.insert clientId client s.clients }) stateRef'
    log $ "Client " <> clientId <> " connected. Total clients: " <> show (Map.size newState.clients)

    -- Send initial connection message
    let
      connectedMsg = unsafeToForeign
        { type: "connected"
        , clientId: clientId
        , message: "PureScript SSE connection established"
        }
    writeSSE res' (jsonStringifyJS connectedMsg)

    -- Create ping data generator
    let
      getPingData = do
        currentState <- Ref.read stateRef'
        currentTime' <- now
        let Milliseconds pingMs = unInstant currentTime'
        pure $ unsafeToForeign
          { type: "ping"
          , clientId: clientId
          , timestamp: pingMs
          , totalClients: Map.size currentState.clients
          }

    -- Create cleanup callback
    let
      cleanup = do
        finalState <- Ref.modify (\s -> s { clients = Map.delete clientId s.clients }) stateRef'
        log $ "Client " <> clientId <> " removed. Remaining: " <> show (Map.size finalState.clients)

    -- Use PureScript implementation for SSE connection setup
    setupSSEConnectionPS req' res' clientId getPingData cleanup

-- Timer management
foreign import data IntervalId :: Type
foreign import setIntervalJS :: Int -> Effect Unit -> Effect IntervalId
foreign import clearIntervalJS :: IntervalId -> Effect Unit

-- Event listeners
foreign import onRequestJS :: Request -> String -> Effect Unit -> Effect Unit
foreign import onResponseJS :: Response -> String -> Effect Unit -> Effect Unit

-- Response state checking
foreign import isResponseClosedJS :: Response -> Effect Boolean
foreign import tryEndResponseJS :: Response -> Effect Unit

-- PureScript implementation of SSE connection setup
setupSSEConnectionPS :: Request -> Response -> String -> Effect Foreign -> Effect Unit -> Effect Unit
setupSSEConnectionPS req res clientId getPingData cleanup = do
  -- Create ref to store interval ID for cleanup
  intervalRef <- Ref.new (Nothing :: Maybe IntervalId)

  -- Create cleanup function
  let
    doCleanup = do
      currentTime <- now
      let Milliseconds ms = unInstant currentTime
      log $ "[" <> show ms <> "] Client " <> clientId <> " disconnected"

      -- Clear the interval
      maybeInterval <- Ref.read intervalRef
      case maybeInterval of
        Just intervalId -> clearIntervalJS intervalId
        Nothing -> pure unit

      -- Call the provided cleanup
      cleanup

      -- End the response safely
      tryEndResponseJS res

  -- Setup ping interval (1000ms = 1 second)
  intervalId <- setIntervalJS 1000 do
    -- Try to send ping, catch any errors
    -- Note: We'll handle errors in the FFI layer if needed
    pingData <- getPingData
    writeSSE res (jsonStringifyJS pingData)
    currentTime <- now
    let Milliseconds ms = unInstant currentTime
    log $ "[" <> show ms <> "] Ping sent to " <> clientId

  -- Store the interval ID for cleanup
  Ref.write (Just intervalId) intervalRef

  -- Register all the critical event handlers
  onRequestJS req "close"
    ( do
        currentTime <- now
        let Milliseconds ms = unInstant currentTime
        log $ "[" <> show ms <> "] Request 'close' event for " <> clientId
        doCleanup
    )

  onRequestJS req "end"
    ( do
        currentTime <- now
        let Milliseconds ms = unInstant currentTime
        log $ "[" <> show ms <> "] Request 'end' event for " <> clientId
        doCleanup
    )

  onRequestJS req "aborted"
    ( do
        currentTime <- now
        let Milliseconds ms = unInstant currentTime
        log $ "[" <> show ms <> "] Request aborted for " <> clientId
        doCleanup
    )

  onResponseJS res "close"
    ( do
        currentTime <- now
        let Milliseconds ms = unInstant currentTime
        log $ "[" <> show ms <> "] Response 'close' event for " <> clientId
        doCleanup
    )

  onResponseJS res "error"
    ( do
        currentTime <- now
        let Milliseconds ms = unInstant currentTime
        log $ "[" <> show ms <> "] Response error for " <> clientId
        doCleanup
    )

-- Client type
type Client =
  { id :: String
  , connectedAt :: Instant
  }

-- Server state
type ServerState =
  { clients :: Map.Map String Client
  , counter :: Int
  }

-- Generate alphabetic IDs (A, B, C, ..., Z, AA, AB, ...)
generateAlphabeticId :: Int -> String
generateAlphabeticId n = go n ""
  where
  go num acc
    | num < 0 = acc
    | otherwise =
        let
          remainder = mod num 26
          quotient = div num 26
          charCode = 65 + remainder
          char = case toEnum charCode of
            Just c -> String.singleton c
            Nothing -> "?"
        in
          if quotient == 0 then char <> acc
          else go (quotient - 1) (char <> acc)

-- Create and start the server
startServer :: Int -> Effect Server
startServer port = do
  -- Initialize state
  stateRef <- Ref.new { clients: Map.empty, counter: 0 }

  -- Create server with unified request handler
  server <- createServerJS (handleRequestPS stateRef)

  -- Start listening
  listenJS port server do
    log $ "🟪 PureScript SSE Server started on port " <> show port
    log "✅ Using direct Node.js HTTP via FFI:"
    log "   - Node handles raw HTTP server creation"
    log "   - PureScript handles ALL request processing"
    log "   - Event listeners properly registered in JavaScript"
    log ""
    log "Expected behavior: IMMEDIATE disconnection detection"
    log ""
    log $ "Endpoints:"
    log $ "  - http://localhost:" <> show port <> "/events (SSE stream)"
    log $ "  - http://localhost:" <> show port <> "/status (Active connections)"
    log $ "  - http://localhost:" <> show port <> "/hello (JSON greeting)"

  pure server