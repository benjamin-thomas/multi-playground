# servant-tutorial

WIP: I should finish this exploration

Run with:

`--warnings` forces running main even if there are warnings

```sh
ghcid --lint -c 'cabal repl exe:servant-demo' -T main --warnings
PORT=4000 ghcid -c 'cabal repl exe:servant-demo' -T main --warnings
```

Source:
https://github.com/commercialhaskell/stack/issues/1765#issuecomment-480559514

---

Run tests with:

```sh
ghcid --lint -c 'cabal repl servant-demo-test' -T main

# https://stackoverflow.com/a/74276514
ghcid --target=servant-demo-test
```

Or

```
cabal repl servant-demo-test
ghci> :main
ghci> :cmd return $ unlines [":reload", ":main"]
```
