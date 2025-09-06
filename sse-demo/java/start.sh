#!/bin/bash

echo "🟠 Starting Java SSE Server (Port 5004)"
echo "Compiling..."
javac SSEServer.java

if [ $? -eq 0 ]; then
    echo "Starting server..."
    exec java SSEServer
else
    echo "Compilation failed!"
    exit 1
fi