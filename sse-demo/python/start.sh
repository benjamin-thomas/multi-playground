#!/bin/bash

echo "🐍 Starting Python SSE Server (Port 5008)"

# Check Python version
if command -v python3 &> /dev/null; then
    PYTHON_CMD="python3"
elif command -v python &> /dev/null; then
    PYTHON_CMD="python"
else
    echo "Error: Python is not installed"
    exit 1
fi

# Install requirements if needed
if [ -s requirements.txt ]; then
    echo "Installing dependencies..."
    $PYTHON_CMD -m pip install -r requirements.txt
fi

echo "Starting server..."
exec $PYTHON_CMD -u server.py