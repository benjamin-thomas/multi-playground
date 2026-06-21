# 99 problems exercises

Problems at: https://ocaml.org/exercises

## Setup

# Terminal 1: ABC
```sh
yarn res:dev
```

# Terminal 2: tests
```sh
yarn test:watch
```

## Notes

### rescript@13.0.0-alpha.4 (pre-release)

Using the alpha instead of stable (12.x) for `rescript watch --clear-screen`.

- Feature request: https://github.com/rescript-lang/rescript/issues/8139 (@cknitt)
- Implemented in: https://github.com/rescript-lang/rescript/pull/8373 (@Bushuo, merged 2026-04-19)

## Tips

Find std lib stuff (List module here)

```sh
rg --type-add 'rescript:*.res' --type-add 'rescript:*.resi' -trescript List node_modules/
```
