#!/bin/bash

echo "🦀 Starting Rust SSE Server (Port 5012)"

# Check if cargo binary exists
if ! command -v cargo &> /dev/null; then
    echo "❌ Cargo not found! Please install Rust first."
    echo "Visit: https://rustup.rs/"
    exit 1
fi

echo "Building project..."
cargo build --release

if [ $? -ne 0 ]; then
    echo "❌ Build failed!"
    exit 1
fi

echo "Starting server..."
exec cargo run --release