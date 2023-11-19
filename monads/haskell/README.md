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

### Option 2a
```
cabal repl
ghci> :!doctest ./src/Monads.hs
```

###  Option 2b

```
cabal repl
ghci> :cmd return $ unlines [":reload", ":!doctest ./src/"]
```

### Option 3 (the best!)

```
$ ghcid --command 'cabal repl' --test ':!doctest ./src/ExploreMonads.hs'

# Or
$ ghcid --command 'cabal repl' --test ':!doctest ./src/'
```

### Option 4

```
cabal repl --with-ghc=doctest
find ./src/ | entr -rc cabal repl --with-ghc=doctest
```
