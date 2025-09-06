#!/bin/bash
set -e

echo "📡 Erlang SSE Server Starting (Port 5014)"
echo "Compiling Erlang modules..."

# Compile Erlang modules
erlc *.erl

echo "Starting Erlang server..."
erl -noshell -eval "main:start(), timer:sleep(infinity)."