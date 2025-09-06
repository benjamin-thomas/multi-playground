#!/bin/bash

echo "🔷 Building F# SSE Server"
echo "================================"

# Check .NET SDK
if ! command -v dotnet &> /dev/null; then
    echo "❌ Error: .NET SDK is not installed"
    echo "Install from: https://dotnet.microsoft.com/download"
    exit 1
fi

echo "✓ .NET version: $(dotnet --version)"

# Clean previous build
echo "Cleaning previous build..."
dotnet clean 2>/dev/null

# Restore dependencies
echo "Restoring dependencies..."
dotnet restore

if [ $? -ne 0 ]; then
    echo "❌ Failed to restore dependencies"
    exit 1
fi

# Build project
echo "Building project..."
dotnet build --configuration Release

if [ $? -eq 0 ]; then
    echo "✅ F# build successful!"
    echo "Run './start.sh' to start the server"
    
    # Show build output location
    echo "✓ Build output: bin/Release/net8.0/"
else
    echo "❌ Build failed!"
    exit 1
fi