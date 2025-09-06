#!/bin/bash

# C SSE Server Startup Script

echo "🔧 C/Mongoose SSE Server Starting (Port 5013)"

# Build if necessary
if [ ! -f sse_server ] || [ main.c -nt sse_server ] || [ mongoose.c -nt sse_server ]; then
    echo "Building C server..."
    make clean && make
    if [ $? -ne 0 ]; then
        echo "Build failed!"
        exit 1
    fi
fi

# Run the server
exec ./sse_server