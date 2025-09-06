#!/bin/bash

echo "🟣 Starting Haskell SSE Server (Port 5005)"

echo "Building with Cabal..."
cabal build

if [ $? -eq 0 ]; then
    echo "Starting server..."
    exec cabal run sse-server
else
    echo "Build failed!"
    exit 1
fi