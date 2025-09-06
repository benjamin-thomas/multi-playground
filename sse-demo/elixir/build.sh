#!/bin/bash

echo "🟢 Building Elixir SSE Server"
echo "================================"

# Check Elixir
if ! command -v elixir &> /dev/null; then
    echo "❌ Error: Elixir is not installed"
    exit 1
fi

echo "✓ Elixir version: $(elixir --version | head -n 1)"

# Check Erlang
if ! command -v erl &> /dev/null; then
    echo "❌ Error: Erlang is not installed (required for Elixir)"
    exit 1
fi

echo "✓ Erlang/OTP version: $(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell)"

# Check mix
if ! command -v mix &> /dev/null; then
    echo "❌ Error: Mix is not available"
    exit 1
fi

echo "✓ Mix available"

# Get dependencies
echo "Fetching dependencies..."
mix deps.get

if [ $? -ne 0 ]; then
    echo "⚠️  Failed to fetch some dependencies"
    echo "Try running: mix local.hex --force"
fi

# Compile dependencies
echo "Compiling dependencies..."
mix deps.compile

# Compile project
echo "Compiling project..."
mix compile

if [ $? -eq 0 ]; then
    echo "✅ Elixir build successful!"
    echo "Run './start.sh' to start the server"
    
    # Show compiled beam files
    echo "✓ Compiled beam files in: _build/dev/lib/"
else
    echo "❌ Build failed!"
    echo "Try running:"
    echo "  mix local.hex --force"
    echo "  mix local.rebar --force"
    exit 1
fi