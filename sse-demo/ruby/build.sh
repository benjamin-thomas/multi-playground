#!/bin/bash

echo "🔴 Building Ruby SSE Server"
echo "================================"

# Check Ruby version
if ! command -v ruby &> /dev/null; then
    echo "❌ Error: Ruby is not installed"
    exit 1
fi

echo "✓ Ruby version: $(ruby --version)"

# Check bundler
if ! command -v bundle &> /dev/null; then
    echo "Installing bundler..."
    gem install bundler
fi

# Install dependencies
echo "Installing dependencies..."
bundle install

if [ $? -eq 0 ]; then
    echo "✅ Ruby build successful!"
    echo "Run './start.sh' to start the server"
else
    echo "❌ Build failed!"
    exit 1
fi