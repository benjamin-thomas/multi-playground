#!/bin/bash

echo "🟢 Building Node.js SSE Server"
echo "================================"

# Check Node.js
if ! command -v node &> /dev/null; then
    echo "❌ Error: Node.js is not installed"
    echo "Install from: https://nodejs.org/"
    exit 1
fi

echo "✓ Node.js version: $(node --version)"

# Check npm
if ! command -v npm &> /dev/null; then
    echo "⚠️  npm not found, but Node.js is installed"
    echo "The server uses only built-in modules, so it should still work"
else
    echo "✓ npm version: $(npm --version)"
fi

# Check JavaScript syntax
echo "Checking JavaScript syntax..."
node -c server.js

if [ $? -eq 0 ]; then
    echo "✓ Syntax check passed"
else
    echo "❌ Syntax errors found in server.js!"
    exit 1
fi

# Install dependencies if package.json exists and has dependencies
if [ -f package.json ]; then
    # Check if there are any dependencies
    if grep -q '"dependencies"' package.json; then
        echo "Installing dependencies..."
        npm install
        
        if [ $? -ne 0 ]; then
            echo "⚠️  Failed to install some dependencies"
        fi
    else
        echo "✓ No external dependencies required"
    fi
fi

# Verify the server can be loaded
echo "Verifying server module..."
node -e "try { require('./server.js'); console.log('✓ Server module loads successfully'); } catch(e) { console.error('❌ Error loading server:', e.message); process.exit(1); }"

if [ $? -eq 0 ]; then
    echo "✅ Node.js build successful!"
    echo "Run './start.sh' to start the server"
else
    echo "❌ Build verification failed!"
    exit 1
fi