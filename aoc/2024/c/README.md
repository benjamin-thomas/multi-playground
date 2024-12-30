## Setup

The following makes the `CS128.cs128-clang-tidy` vscode extension happy

```sh
ln -s build/compile_commands.json .
```

It also makes the `clang-tidy` binary happy:

```sh
clang-tidy ./day01.c
```

## Build

```sh
# Step 1: set the build context, run once
cmake -S . -B build
TARGET=prod cmake -S . -B build

# Step 2: Actually execute the build
cmake --build ./build

# Rebuild fully
rm -r ./build && cmake -S . -B ./build && cmake --build ./build
rm -r ./build && TARGET=prod cmake -S . -B ./build && cmake --build ./build
```

## vscode tips

- ctrl+shift+b: build the project
- f5: debug the current file
