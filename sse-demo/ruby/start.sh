#!/bin/bash

echo "🔴 Starting Ruby SSE Server (Port 5003)"
echo "Installing dependencies..."
bundle install

echo "Starting server..."
exec ruby server.rb