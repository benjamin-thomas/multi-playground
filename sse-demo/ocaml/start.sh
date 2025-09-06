#!/bin/bash

echo "🟤 Starting OCaml SSE Server (Port 5007)"

# Check if opam/dune is installed
if command -v opam &> /dev/null && command -v dune &> /dev/null; then
    echo "Installing dependencies..."
    opam install -y dream lwt yojson dune
    
    echo "Building with Dune..."
    dune build
    
    if [ $? -eq 0 ]; then
        echo "Starting server..."
        exec dune exec ./server.exe
    else
        echo "Build failed!"
        exit 1
    fi
else
    echo "Warning: Dune not found. Trying to run with ocaml directly..."
    echo "Note: You may need to install dependencies first:"
    echo "  opam install dream lwt yojson"
    exec ocaml server.ml
fi