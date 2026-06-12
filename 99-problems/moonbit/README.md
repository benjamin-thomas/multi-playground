# 99 problems exercises

Problems at: https://ocaml.org/exercises

```sh
# moon test --watch
rg --files | entr -c moon test
```

## Layout

Each exercise lives in its own package (`src/exNN/`) so solutions stay
isolated. Use `src/ex00/` as a starting template — copy the directory
and rename.

## Reusing code from another exercise

To call into a previous exercise, opt in explicitly from the importing
exercise's `moon.pkg`:

```jsonc
import {
  "moonbitlang/core/list",
  "moonbitlang/core/debug",
  "99-problems/src/ex01",
}
```

Then reference the function via the package alias (`@ex01.last(...)`).
The target function must be declared `pub` to be visible across
packages.