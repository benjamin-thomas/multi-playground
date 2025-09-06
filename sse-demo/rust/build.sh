#!/bin/bash

echo "🦀 Building Rust SSE Server"
echo "================================"

# Check for cargo binary
if ! command -v cargo &> /dev/null; then
    echo "❌ Cargo not found! Please install Rust first."
    echo "Visit: https://rustup.rs/"
    exit 1
fi

echo "✓ Rust version: $(rustc --version)"
echo "✓ Cargo version: $(cargo --version)"

echo "Building project..."
cargo build --release

if [ $? -eq 0 ]; then
    echo "✅ Rust build successful!"
    echo "Run './start.sh' to start the server"
    
    # Check if build output exists
    if [ -f "target/release/sse_server" ]; then
        echo "✓ Executable: target/release/sse_server"
        ls -lh target/release/sse_server | cut -d' ' -f5- | awk '{print "✓ Size: " $1 " " $2}'
    fi
else
    echo "❌ Build failed!"
    exit 1
fi