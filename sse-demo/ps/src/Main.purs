module Main where

{-
  HTTPurple SSE Implementation - Demonstrates Disconnection Detection Limitation
  
  This implementation shows why HTTPurple cannot reliably detect SSE client disconnections:
  1. HTTPurple doesn't expose the underlying Node.js IncomingMessage object
  2. We can't attach event listeners for 'close', 'end', or 'aborted' events  
  3. Relying on stream write failures is unreliable due to buffering
  
  Expected behavior: Delayed or no disconnection detection
-}

import Prelude

import Data.Array as Array
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, delay, forkAff, killFiber, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (try)
import Data.Either (Either(..))
import Effect.Now (now)
import Data.DateTime.Instant (unInstant)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import HTTPurple (Method(..), Request, ResponseM, header, methodNotAllowed, ok, response', serve, RouteDuplex', segment, noArgs, mkRoute, (/))
import Routing.Duplex as RD
import Data.Generic.Rep (class Generic)
import HTTPurple.Headers (empty)
import Node.Encoding (Encoding(..))
import Node.Stream as Stream

-- SSE Client type
type SSEClient =
  { id :: String
  , stream :: Stream.Duplex
  , pingFiber :: Fiber Unit
  , connectedAt :: Milliseconds
  }

-- Generate alphabetic IDs (A, B, C, ..., Z, AA, AB, ...)
numberToAlphabeticId :: Int -> String
numberToAlphabeticId n = go n ""
  where
  go num acc
    | num < 0 = acc
    | otherwise =
        let
          remainder = num `mod` 26
          quotient = num `div` 26
          char = case remainder of
            0 -> "A"
            1 -> "B"
            2 -> "C"
            3 -> "D"
            4 -> "E"
            5 -> "F"
            6 -> "G"
            7 -> "H"
            8 -> "I"
            9 -> "J"
            10 -> "K"
            11 -> "L"
            12 -> "M"
            13 -> "N"
            14 -> "O"
            15 -> "P"
            16 -> "Q"
            17 -> "R"
            18 -> "S"
            19 -> "T"
            20 -> "U"
            21 -> "V"
            22 -> "W"
            23 -> "X"
            24 -> "Y"
            _ -> "Z"
        in
          if quotient == 0 then char <> acc
          else go (quotient - 1) (char <> acc)

-- Simple JSON encoding for SSE events
encodeSSEEvent :: String -> String -> String -> String
encodeSSEEvent eventType clientId message =
  "data: {\"type\":\"" <> eventType <> "\",\"clientId\":\"" <> clientId <> "\",\"message\":\"" <> message <> "\"}\n\n"

encodePingEvent :: String -> Number -> Int -> String
encodePingEvent clientId timestamp totalClients =
  "data: {\"type\":\"ping\",\"clientId\":\"" <> clientId <> "\",\"timestamp\":" <> show timestamp <> ",\"totalClients\":" <> show totalClients <> "}\n\n"

-- Simple Route type
data Route = Home | Events | Status

derive instance genericRoute :: Generic Route _

-- Route definition
route :: RouteDuplex' Route
route = mkRoute
  { "Home": noArgs
  , "Events": "events" / noArgs
  , "Status": "status" / noArgs
  }

-- Router
router :: Ref (Array SSEClient) -> Ref Int -> Request Route -> ResponseM
router clientsRef counterRef request =
  case request.route of
    Events ->
      case request.method of
        Get -> handleEvents clientsRef counterRef
        Options -> addCorsHeaders $ ok ""
        _ -> methodNotAllowed
    Status ->
      case request.method of
        Get -> handleStatus clientsRef
        Options -> addCorsHeaders $ ok ""
        _ -> methodNotAllowed
    Home ->
      case request.method of
        Options -> addCorsHeaders $ ok ""
        _ -> addCorsHeaders $ ok "HTTPurple SSE Demo Server (Port 5001)\n\nEndpoints:\n- /events (SSE)\n- /status (JSON)"

addCorsHeaders :: ResponseM -> ResponseM
addCorsHeaders responseM = do
  response <- responseM
  pure $ response { headers = response.headers <> header "Access-Control-Allow-Origin" "*" }

-- Handle SSE events endpoint
handleEvents :: Ref (Array SSEClient) -> Ref Int -> ResponseM
handleEvents clientsRef counterRef = do
  -- Generate client ID
  clientId <- liftEffect $ do
    counter <- Ref.read counterRef
    Ref.write (counter + 1) counterRef
    pure $ numberToAlphabeticId counter

  liftEffect $ log $ "[HTTPurple] Client " <> clientId <> " connecting (LIMITED DISCONNECTION DETECTION)"

  -- Create stream
  stream <- liftEffect Stream.newPassThrough

  -- Get connection timestamp
  instant <- liftEffect now
  let connectedAt = unInstant instant

  -- Send initial connection message
  _ <- liftEffect $ Stream.writeString stream UTF8 $
    encodeSSEEvent "connected" clientId "HTTPurple SSE (unreliable disconnection)"

  -- Start ping loop
  pingFiber <- forkAff $ pingLoop clientId stream clientsRef

  -- Create client record
  let client = { id: clientId, stream, pingFiber, connectedAt }

  -- Add to clients
  liftEffect $ do
    clients <- Ref.read clientsRef
    let newClients = Array.snoc clients client
    Ref.write newClients clientsRef
    log $ "[HTTPurple] Client " <> clientId <> " added. Total: " <> show (Array.length newClients)

  -- Return SSE response
  let
    sseHeaders = empty
      <> header "Content-Type" "text/event-stream"
      <> header "Cache-Control" "no-cache"
      <> header "Connection" "keep-alive"
      <> header "Access-Control-Allow-Origin" "*"

  response' 200 sseHeaders stream

  where
  -- LIMITATION: This ping loop can only detect disconnections through write failures
  -- which are unreliable due to Node.js stream buffering
  pingLoop :: String -> Stream.Duplex -> Ref (Array SSEClient) -> Aff Unit
  pingLoop clientId stream clientsRef = do
    delay (Milliseconds 1000.0)

    writeResult <- liftEffect $ do
      instant <- now
      clients <- Ref.read clientsRef
      let Milliseconds timestamp = unInstant instant
      log $ "[HTTPurple] Sending ping to " <> clientId <> " (relying on write failure detection)"

      result <- try $ Stream.writeString stream UTF8 $
        encodePingEvent clientId timestamp (Array.length clients)

      case result of
        Right _ -> do
          log $ "[HTTPurple] Ping sent to " <> clientId <> " (may not detect disconnection immediately)"
          pure true
        Left err -> do
          log $ "[HTTPurple] Write failed for " <> clientId <> ": " <> show err
          log $ "[HTTPurple] Removing client " <> clientId <> " due to write failure"
          -- Remove client
          clients <- Ref.read clientsRef
          let remainingClients = Array.filter (\c -> c.id /= clientId) clients
          Ref.write remainingClients clientsRef
          log $ "[HTTPurple] Client " <> clientId <> " removed. Remaining: " <> show (Array.length remainingClients)
          pure false

    -- Continue loop if write succeeded
    when writeResult $ pingLoop clientId stream clientsRef

-- Handle status endpoint
handleStatus :: Ref (Array SSEClient) -> ResponseM
handleStatus clientsRef = do
  clients <- liftEffect $ Ref.read clientsRef
  let
    status =
      "{\n"
        <> "  \"activeConnections\": "
        <> show (Array.length clients)
        <> ",\n"
        <> "  \"clients\": [\n"
        <>
          ( clients
              # Array.mapWithIndex
                  ( \i client ->
                      "    {\"id\": \"" <> client.id <> "\", \"connectedAt\": " <> show client.connectedAt <> "}" <>
                        if i == Array.length clients - 1 then "" else ","
                  )
              # Array.intercalate "\n"
          )
        <> "\n"
        <> "  ],\n"
        <> "  \"implementation\": \"HTTPurple (unreliable disconnection detection)\"\n"
        <>
          "}"

  addCorsHeaders $ ok status

main :: Effect Unit
main = do
  clientsRef <- Ref.new []
  counterRef <- Ref.new 0

  log ""
  log "🟡 HTTPurple SSE Server Starting (Port 5001)"
  log "⚠️  This implementation demonstrates HTTPurple's limitation:"
  log "   - Cannot access underlying Node.js request object"
  log "   - Cannot attach event listeners for proper disconnection detection"
  log "   - Relies on unreliable stream write failure detection"
  log ""
  log "Expected behavior: Delayed or missed disconnection detection"
  log ""

  void $ serve { port: 5001 } { route: route, router: router clientsRef counterRef }