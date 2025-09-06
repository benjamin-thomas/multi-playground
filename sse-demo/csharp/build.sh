#!/bin/bash

echo "💙 Building C# SSE Server"
echo "================================"

# Check .NET SDK
if ! command -v dotnet &> /dev/null; then
    echo "❌ Error: .NET SDK is not installed"
    echo "Install from: https://dotnet.microsoft.com/download"
    exit 1
fi

echo "✓ .NET version: $(dotnet --version)"

# Show installed SDKs
echo "Installed SDKs:"
dotnet --list-sdks | head -5

# Clean previous build
echo "Cleaning previous build..."
dotnet clean 2>/dev/null
rm -rf bin obj

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
    echo "✅ C# build successful!"
    echo "Run './start.sh' to start the server"
    
    # Show build output location
    echo "✓ Build output: bin/Release/net8.0/"
    
    # List generated files
    if [ -d "bin/Release/net8.0" ]; then
        echo "Generated files:"
        ls -la bin/Release/net8.0/*.dll 2>/dev/null | head -5
    fi
else
    echo "❌ Build failed!"
    exit 1
fi