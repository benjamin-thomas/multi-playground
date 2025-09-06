#!/bin/bash
set -e

echo "🟪 PureScript SSE Server Starting (Port 5001)"
echo "Building PureScript project..."

# Build the PureScript project
spago build

echo "Starting server..."
node main.js