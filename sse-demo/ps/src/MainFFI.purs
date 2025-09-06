module MainFFI where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Server (startServer)

main :: Effect Unit
main = do
  log ""
  log "🟪 PureScript SSE Server Starting (Port 5001)"

  -- Start the server on port 5001
  _ <- startServer 5001

  -- The Node.js event loop will keep the process running
  pure unit