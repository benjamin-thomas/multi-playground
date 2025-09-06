#!/bin/bash

echo "🟤 Building OCaml SSE Server"
echo "================================"

# Check OCaml
if ! command -v ocaml &> /dev/null; then
    echo "❌ Error: OCaml is not installed"
    exit 1
fi

echo "✓ OCaml version: $(ocaml -version)"

# Check opam
if command -v opam &> /dev/null; then
    echo "✓ opam version: $(opam --version)"
    
    # Initialize opam if needed
    if [ ! -d ~/.opam ]; then
        echo "Initializing opam..."
        opam init --auto-setup --yes
        eval $(opam env)
    else
        eval $(opam env)
    fi
    
    # Install dependencies
    echo "Installing dependencies..."
    opam install -y dune dream lwt yojson
    
    if [ $? -ne 0 ]; then
        echo "⚠️  Some dependencies may have failed to install"
    fi
else
    echo "⚠️  opam not found. Install it for proper dependency management."
fi

# Check dune
if command -v dune &> /dev/null; then
    echo "✓ dune version: $(dune --version)"
    
    # Build with dune
    echo "Building with dune..."
    dune build
    
    if [ $? -eq 0 ]; then
        echo "✅ OCaml build successful!"
        echo "Run './start.sh' to start the server"
        echo "✓ Executable: _build/default/server.exe"
    else
        echo "⚠️  Dune build failed"
        echo "Make sure dependencies are installed:"
        echo "  opam install dream lwt yojson"
    fi
else
    echo "⚠️  Dune not found. Install it with: opam install dune"
    echo "You can try running directly with: ocaml server.ml"
fi