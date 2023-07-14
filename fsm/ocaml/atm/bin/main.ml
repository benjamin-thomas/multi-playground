let usage () =
  let open State in
  Ready
  |> transition' InsertCard
  |> transition' (VerifyCard (Ok ()))
  |> transition' (VerifyPIN (Error "Too small. Remaining 3."))
  |> transition' (VerifyPIN (Error "Invalid PIN. Remaining 2."))
  |> transition' (VerifyPIN (Ok ()))
  |> transition' Withdraw
  |> transition' Withdraw
  |> transition' Withdraw
  |> transition' RequestEject
;;

let () =
  ()
  ; print_endline "Demonstrating running the state machine..."
  ; usage () |> ignore
;;

(*
Or explore in the REPL

utop # Ready
|> transition InsertCard
|> transition (VerifyCard (Ok ()))
|> transition (VerifyPIN (Ok ()))
|> transition Withdraw
|> transition RequestEject
;;
- : state = Ready

 *)
