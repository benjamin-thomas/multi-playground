#!/bin/bash

echo "🟢 Starting Node.js SSE Server (Port 5000)"

# Check if Node.js is installed
if ! command -v node &> /dev/null; then
    echo "Error: Node.js is not installed"
    exit 1
fi

echo "Node.js version: $(node --version)"

# Install dependencies if package-lock.json exists and node_modules doesn't
if [ -f package-lock.json ] && [ ! -d node_modules ]; then
    echo "Installing dependencies..."
    npm install
fi

echo "Starting server..."
exec node server.js