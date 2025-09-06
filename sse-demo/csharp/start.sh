#!/bin/bash

echo "💙 Starting C# SSE Server (Port 5009)"

# Check if dotnet is installed
if ! command -v dotnet &> /dev/null; then
    echo "Error: .NET SDK is not installed"
    exit 1
fi

echo "Building project..."
dotnet build

if [ $? -eq 0 ]; then
    echo "Starting server..."
    exec dotnet run
else
    echo "Build failed!"
    exit 1
fi