#!/bin/bash

echo "🟠 Building Java SSE Server"
echo "================================"

# Check Java version
if ! command -v java &> /dev/null; then
    echo "❌ Error: Java is not installed"
    exit 1
fi

if ! command -v javac &> /dev/null; then
    echo "❌ Error: Java compiler (javac) is not installed"
    exit 1
fi

echo "✓ Java version: $(java -version 2>&1 | head -n 1)"
echo "✓ Javac version: $(javac -version 2>&1)"

# Clean previous build
echo "Cleaning previous build..."
rm -f *.class

# Compile
echo "Compiling SSEServer.java..."
javac SSEServer.java

if [ $? -eq 0 ]; then
    echo "✅ Java build successful!"
    echo "Run './start.sh' to start the server"
    
    # Verify class file was created
    if [ -f "SSEServer.class" ]; then
        echo "✓ SSEServer.class created"
    fi
else
    echo "❌ Build failed!"
    exit 1
fi