# Elm Reified Effects Demo

This project demonstrates two approaches to the "Effect Pattern" in Elm, comparing them to the Haskell implementations.

## Two Versions

### Main0.elm - Callbacks Version
- Effects contain callbacks: `SaveUser User (Int -> Effect)`
- Flow stays together in one `Msg` branch
- Cannot be fully tested/compared (callbacks are opaque)
- Similar to Haskell Main0.hs

### Main1.elm - Scattered Actions Version
- Effects are pure data with NO callbacks
- Flow scattered across multiple `Msg` branches (`RegisterClicked`, `UserSaved`)
- Fully testable and comparable
- Similar to Haskell Main1.hs
- **This is the standard Elm approach**

## Building

```bash
# Compile Main0 (callbacks version)
elm make src/Main0.elm --output=main0.js

# Compile Main1 (scattered actions version)
elm make src/Main1.elm --output=main1.js

# Or compile both at once
elm make src/Main0.elm --output=main0.js && elm make src/Main1.elm --output=main1.js
```

## Running

Open `index.html` in a browser. Use the buttons to switch between versions.

## Key Differences from Haskell

1. **Callback invocation**: In Elm, we can't directly invoke callbacks inside `Cmd` like we can in Haskell's `IO`. We use `EffectCompleted` messages to route callback results back through `update`.

2. **Type system**: Elm's simpler type system means we don't need `Functor`/`Applicative`/`Monad` instances like in the Haskell Free monad version.

3. **Testing**: Both versions are harder to test than pure `Cmd Msg` because we're building our own effect system, but Main1 is fully testable since effects are pure data.

## Why Use Custom Effects Instead of Cmd Msg?

Custom Effect types give you:
- **Inspectability**: Can see what effects will run before running them
- **Testability**: Can compare expected vs actual effects in tests
- **Serialization**: Can send effects over the wire or store them
- **Mocking**: Can run effects differently in tests vs production

Most Elm apps use `Cmd Msg` because it's simpler and integrates with the platform. Custom effects are useful for complex apps that need more control.
