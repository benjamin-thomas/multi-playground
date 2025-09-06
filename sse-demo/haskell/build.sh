#!/bin/bash

echo "🟣 Building Haskell SSE Server"
echo "================================"

# Check GHC version
if ! command -v ghc &> /dev/null; then
    echo "⚠️  Warning: GHC is not installed"
    if ! command -v runhaskell &> /dev/null; then
        echo "❌ Error: Neither GHC nor runhaskell is available"
        exit 1
    else
        echo "✓ Will use runhaskell for execution"
    fi
else
    echo "✓ GHC version: $(ghc --version)"
fi

# Check Cabal
if command -v cabal &> /dev/null; then
    echo "✓ Cabal version: $(cabal --version | head -n 1)"
    
    # Update cabal package list if needed
    echo "Updating package list..."
    cabal update
    
    # Configure and build
    echo "Configuring project..."
    cabal configure
    
    echo "Building project..."
    cabal build
    
    if [ $? -eq 0 ]; then
        echo "✅ Haskell build successful!"
        echo "Run './start.sh' to start the server"
    else
        echo "⚠️  Cabal build failed, but you can still run with 'runhaskell server.hs'"
    fi
else
    echo "⚠️  Cabal not found. Install it for proper dependency management."
    echo "You can still run the server with: runhaskell server.hs"
    
    # Check if required packages are available
    echo "Checking for required packages..."
    ghc-pkg list wai 2>/dev/null | grep -q wai && echo "✓ wai found" || echo "⚠️  wai not found"
    ghc-pkg list warp 2>/dev/null | grep -q warp && echo "✓ warp found" || echo "⚠️  warp not found"
    ghc-pkg list http-types 2>/dev/null | grep -q http-types && echo "✓ http-types found" || echo "⚠️  http-types not found"
fi