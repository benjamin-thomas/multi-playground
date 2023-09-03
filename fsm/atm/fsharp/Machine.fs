module Machine

type State =
    | Ready
    | ValidatingCard
    | ValidatingPIN
    | Session

type Event =
    | InsertCard
    | VerifyCard of Result<unit, string>
    | VerifyPIN of Result<unit, string>
    | Withdraw
    | RequestEject

let transit event state =
    match state, event with
    | Ready, InsertCard -> ValidatingCard
    | ValidatingCard, VerifyCard res ->
        match res with
        | Error err ->
            printfn $"Invalid card, eject! (err: %s{err})"
            Ready
        | Ok() -> ValidatingPIN
    | ValidatingPIN, VerifyPIN res ->
        match res with
        | Error err ->
            printfn $"Invalid PIN, try again! (err: %s{err})"
            ValidatingPIN
        | Ok() -> Session
    | Session, Withdraw -> Session
    | Session, RequestEject -> Ready
    | _ ->
        printfn $"Invalid transition: %A{event} cannot be run from %A{state}"
        state


let printState state =
    match state with
    | Ready -> printfn "ATM is ready!"
    | ValidatingCard -> printfn "Validating card..."
    | ValidatingPIN -> printfn "Validating PIN..."
    | Session -> printfn "ATM session in progress."

let transit' event currState =
    let nextState = transit event currState
    printState nextState
    nextState
