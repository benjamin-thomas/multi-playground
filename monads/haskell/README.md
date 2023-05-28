## SETUP

```
cabal update && cabal install doctest
```

### Note to self

I had trouble/conflicts with a prior `doctest` installation (tied to a prior GHC installation).
I had to `rm -rf ~/.cabal` and redo the setup steps to make `doctest` happy.

---

## Run the doctests

### Option 1

```
cabal exec doctest src/
```

### Option 2
```
cabal repl
ghci> :!doctest ./src/Monads.hs
```

### Option 3

```
ghcid --command 'cabal repl' --test ':!doctest ./src/Monads.hs'
```

### Option 4

```
cabal repl --with-ghc=doctest
find ./src/ | entr -rc cabal repl --with-ghc=doctest
```
