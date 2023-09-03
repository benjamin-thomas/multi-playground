```mermaid
stateDiagram-v2
    [*] --> ValidatingCard : insertCard
    ValidatingCard --> ValidatingPin : verifyCard (Ok)
    ValidatingCard --> [*] : verifyCard (Err)
    ValidatingPin --> ValidatingPin : verifyPin (Err)
    ValidatingPin --> Session : verifyPin (Ok)
    ValidatingPin --> [*] : requestEject
    Session --> Session : withdraw
    Session --> [*] : requestEject
```