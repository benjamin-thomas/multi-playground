module Main2 where

{-
  Pure FFI Implementation - Proper SSE Disconnection Detection
  
  This implementation demonstrates that PureScript CAN handle SSE disconnections
  properly when we have direct access to Node.js request/response objects:
  1. We create our own HTTP server via FFI
  2. We have full access to Node.js IncomingMessage and ServerResponse objects
  3. We can attach event listeners for immediate disconnection detection
  
  Expected behavior: Immediate disconnection detection, just like the pure JS version
-}

import Prelude

import Data.Array as Array
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, delay, forkAff, killFiber, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref

-- FFI declarations

-- Raw request info from Node.js
type RawRequestInfo =
  { method :: String
  , url :: String
  , pathname :: String
  , rawRequest :: RawRequest
  , rawResponse :: RawResponse
  }

-- Opaque types for Node.js objects
foreign import data RawRequest :: Type
foreign import data RawResponse :: Type
foreign import data HTTPServer :: Type

-- FFI functions
foreign import createSSEServer
  :: Int
  -> (RawRequestInfo -> Effect Unit)
  -> Effect HTTPServer

foreign import generateAlphabeticId :: Int -> String

foreign import setupSSEConnection
  :: RawResponse
  -> String
  -> Effect RawResponse

foreign import attachDisconnectionHandlers
  :: RawRequest
  -> RawResponse
  -> String
  -> Effect Unit
  -> Effect (Effect Unit)

foreign import sendSSEPing
  :: RawResponse
  -> String
  -> Number
  -> Int
  -> Effect { success :: Boolean, error :: String }

foreign import sendJSONResponse
  :: RawResponse
  -> String
  -> Effect Unit

foreign import send404Response
  :: RawResponse
  -> Effect Unit

foreign import getCurrentTimestamp :: Effect Number

-- SSE Client type
type SSEClient =
  { id :: String
  , response :: RawResponse
  , pingFiber :: Unit
  , cleanup :: Effect Unit
  , connectedAt :: Milliseconds
  }

-- Request handler
handleRequest :: Ref (Array SSEClient) -> Ref Int -> RawRequestInfo -> Effect Unit
handleRequest clientsRef counterRef reqInfo =
  case reqInfo.pathname of
    "/events" -> handleEvents clientsRef counterRef reqInfo
    "/status" -> handleStatus clientsRef reqInfo.rawResponse
    _ -> send404Response reqInfo.rawResponse

-- Handle SSE events endpoint  
handleEvents :: Ref (Array SSEClient) -> Ref Int -> RawRequestInfo -> Effect Unit
handleEvents clientsRef counterRef reqInfo = do
  -- Generate client ID
  counter <- Ref.read counterRef
  Ref.write (counter + 1) counterRef
  let clientId = generateAlphabeticId counter

  log $ "[FFI] Client " <> clientId <> " connecting (PROPER DISCONNECTION DETECTION)"

  -- Setup SSE connection
  response <- setupSSEConnection reqInfo.rawResponse clientId

  -- Get connection timestamp
  timestamp <- getCurrentTimestamp
  let connectedAt = Milliseconds timestamp

  -- Setup disconnection handlers - THIS IS THE KEY DIFFERENCE FROM HTTPURPLE
  cleanup <- attachDisconnectionHandlers reqInfo.rawRequest response clientId do
    log $ "[FFI] Client " <> clientId <> " disconnected via event handler"
    -- Remove from clients array
    clients <- Ref.read clientsRef
    let remainingClients = Array.filter (\c -> c.id /= clientId) clients
    Ref.write remainingClients clientsRef
    log $ "[FFI] Client " <> clientId <> " removed from array. Remaining: " <> show (Array.length remainingClients)

  -- Start ping loop
  pingFiber <- launchAff_ $ pingLoop clientId response clientsRef

  -- Create client record (note: we can't store the actual fiber since launchAff_ returns Unit)
  let client = { id: clientId, response, pingFiber: unit, cleanup, connectedAt }

  -- Add to clients
  clients <- Ref.read clientsRef
  let newClients = Array.snoc clients client
  Ref.write newClients clientsRef
  log $ "[FFI] Client " <> clientId <> " added. Total: " <> show (Array.length newClients)

  where
  -- Ping loop - even if write fails, disconnection will be caught by event handlers
  pingLoop :: String -> RawResponse -> Ref (Array SSEClient) -> Aff Unit
  pingLoop clientId response clientsRef = do
    delay (Milliseconds 1000.0)

    writeResult <- liftEffect $ do
      clients <- Ref.read clientsRef
      timestamp <- getCurrentTimestamp
      log $ "[FFI] Sending ping to " <> clientId <> " (with proper event-based disconnection detection)"

      result <- sendSSEPing response clientId timestamp (Array.length clients)

      if result.success then do
        log $ "[FFI] Ping sent to " <> clientId <> " successfully"
        pure true
      else do
        log $ "[FFI] Ping write failed for " <> clientId <> ": " <> result.error
        log $ "[FFI] Note: Event handlers should have already cleaned up " <> clientId
        pure false

    -- Continue pinging if write succeeded
    when writeResult $ pingLoop clientId response clientsRef

-- Handle status endpoint
handleStatus :: Ref (Array SSEClient) -> RawResponse -> Effect Unit
handleStatus clientsRef response = do
  clients <- Ref.read clientsRef
  let
    clientList = clients
      # Array.mapWithIndex
          ( \i client ->
              "{\"id\": \"" <> client.id <> "\", \"connectedAt\": " <> show client.connectedAt <> "}" <>
                if i == Array.length clients - 1 then "" else ","
          )
      # Array.intercalate "\n    "

    status =
      "{\n"
        <> "  \"activeConnections\": "
        <> show (Array.length clients)
        <> ",\n"
        <> "  \"clients\": [\n"
        <> "    "
        <> clientList
        <> "\n"
        <> "  ],\n"
        <> "  \"implementation\": \"Pure FFI (proper event-based disconnection detection)\"\n"
        <>
          "}"

  sendJSONResponse response status

main :: Effect Unit
main = do
  clientsRef <- Ref.new []
  counterRef <- Ref.new 0

  log ""
  log "🔵 Pure FFI SSE Server Starting (Port 5002)"
  log "✅ This implementation has proper disconnection detection:"
  log "   - Direct access to Node.js request/response objects via FFI"
  log "   - Proper event listeners attached (close, end, aborted)"
  log "   - Immediate disconnection detection just like pure JavaScript"
  log ""
  log "Expected behavior: Immediate and reliable disconnection detection"
  log ""

  void $ createSSEServer 5002 (handleRequest clientsRef counterRef)