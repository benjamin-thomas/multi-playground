#!/bin/bash

echo "✨ Building Gleam SSE Server"
echo "================================"

# Check for gleam binary
if ! command -v gleam &> /dev/null; then
    echo "❌ Gleam not found! Please install gleam first."
    echo "Visit: https://gleam.run/getting-started/installing/"
    exit 1
fi

echo "✓ Gleam version: $(gleam --version)"

echo "Installing dependencies..."
gleam deps download

echo "Building project..."
gleam build

if [ $? -eq 0 ]; then
    echo "✅ Gleam build successful!"
    echo "Run './start.sh' to start the server"
    
    # Check if build output exists
    if [ -d "build/dev/erlang/sse_server" ]; then
        echo "✓ Build output: build/dev/erlang/sse_server/"
    fi
else
    echo "❌ Build failed!"
    exit 1
fi