#!/bin/bash

# Start Gleam SSE server
echo "Installing dependencies..."
gleam deps download

echo "Building project..."
gleam build

echo "Starting server..."
gleam run
