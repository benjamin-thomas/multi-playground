(*
https://mermaid.live/edit

stateDiagram-v2
    [*] --> ValidatingCard : insertCard
    ValidatingCard --> ValidatingPin : verifyCard (Ok)
    ValidatingCard --> [*] : verifyCard (Err)
    ValidatingPin --> ValidatingPin : verifyPin (Err)
    ValidatingPin --> Session : verifyPin (Ok)
    ValidatingPin --> [*] : requestEject
    Session --> Session : withdraw
    Session --> [*] : requestEject

*)




open Machine 
let usage () =
    Ready
    |> transit' InsertCard
    |> transit' (VerifyCard(Ok()))
    |> transit' (VerifyPIN(Error "Too small. Remaining 3."))
    |> transit' (VerifyPIN(Error "Invalid PIN. Remaining 2."))
    |> transit' (VerifyPIN(Ok()))
    |> transit' Withdraw
    |> transit' Withdraw
    |> transit' Withdraw
