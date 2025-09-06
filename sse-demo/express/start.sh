#!/bin/bash
set -e

echo "🚂 Express SSE Server Starting (Port 5015)"
echo "Installing dependencies..."
npm install

echo "Starting Express server..."
node server.js