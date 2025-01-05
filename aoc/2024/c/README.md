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
rm -rf ./build && cmake -S . -B ./build && cmake --build ./build
rm -rf ./build && TARGET=prod cmake -S . -B ./build && cmake --build ./build
```

## vscode tips

- ctrl+shift+b: build the project
- f5: debug the current file

## Command line tips

- continuously build (terminal 1):
  - rg --files | entr -c cmake --build ./build
- continuously lint (terminal 2):
  - rg --files -t c | entr -c clang-tidy /_

---

## GDB tricks

Use enter to repeat the last command

- br 14 # break a tile 14
- c # short for continue
- enter
- enter
- enter

When in a loop, use `display` to print many variables on break

```
display {i, size} # shows 2 (related) values
display {xs[i], ys[i]} # shows 2 (related) array values
p *ys@size # display array values
p *xs@3 # show the first 3 array values
```

---

Remove break points

- info br
  - del NUM

---

Remove display value(s)

- display
- undisplay NUM

---

Reload file (after recompilation)

- gdb ./my_binary
- set confirm off # remove annoying confirmation
- file ./my_binary # reload

---

Find the source of a segfault

- enter gdb
- run
- see the segfault
- bt
- observe the last executed line

---

Always execute from the debugger

- gdb ./my_binary
- define go
  - !clear
  - file ./my_binary # always reload
  - run # then run
  - end
- echo "Then from now on, just execute 'go'"
- go
