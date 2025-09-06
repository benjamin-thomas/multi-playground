#!/bin/bash

echo "🐍 Building Python SSE Server"
echo "================================"

# Check Python
if command -v python3 &> /dev/null; then
    PYTHON_CMD="python3"
    PIP_CMD="pip3"
elif command -v python &> /dev/null; then
    PYTHON_CMD="python"
    PIP_CMD="pip"
else
    echo "❌ Error: Python is not installed"
    exit 1
fi

echo "✓ Python version: $($PYTHON_CMD --version)"

# Check pip
if ! command -v $PIP_CMD &> /dev/null; then
    echo "⚠️  pip not found, trying python -m pip"
    PIP_CMD="$PYTHON_CMD -m pip"
fi

# Check syntax
echo "Checking Python syntax..."
$PYTHON_CMD -m py_compile server.py

if [ $? -eq 0 ]; then
    echo "✓ Syntax check passed"
else
    echo "❌ Syntax errors found!"
    exit 1
fi

# Install/upgrade pip if needed
echo "Ensuring pip is up to date..."
$PIP_CMD install --upgrade pip 2>/dev/null || true

# Install dependencies if requirements.txt has content
if [ -s requirements.txt ]; then
    echo "Installing dependencies from requirements.txt..."
    $PIP_CMD install -r requirements.txt
else
    echo "✓ No external dependencies required (using standard library only)"
fi

# Create __pycache__ if it doesn't exist (for faster imports)
echo "Pre-compiling Python bytecode..."
$PYTHON_CMD -m compileall . -q

echo "✅ Python build successful!"
echo "Run './start.sh' to start the server"