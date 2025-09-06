#!/bin/bash

echo "🟢 Starting Elixir SSE Server (Port 5010)"

# Check if Elixir is installed
if ! command -v elixir &> /dev/null; then
    echo "Error: Elixir is not installed"
    exit 1
fi

# Check if mix is available
if command -v mix &> /dev/null; then
    echo "Installing dependencies..."
    mix deps.get
    
    echo "Compiling..."
    mix compile
    
    if [ $? -eq 0 ]; then
        echo "Starting server..."
        exec mix run --no-halt
    else
        echo "Compilation failed!"
        exit 1
    fi
else
    echo "Starting server directly..."
    exec elixir server.ex
fi