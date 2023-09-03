type state = Ready | ValidatingCard | ValidatingPIN | Session

let string_of_state = function
  | Ready -> "Ready"
  | ValidatingCard -> "ValidatingCard"
  | ValidatingPIN -> "ValidatingPIN"
  | Session -> "Session"
;;

type event =
  | InsertCard
  | VerifyCard of (unit, string) result
  | VerifyPIN of (unit, string) result
  | Withdraw
  | RequestEject

let string_of_event = function
  | InsertCard -> "InsertCard"
  | VerifyCard (Ok ()) -> "VerifyCard OK"
  | VerifyCard (Error _) -> "VerifyCard ERR"
  | VerifyPIN (Ok ()) -> "VerifyPIN OK"
  | VerifyPIN (Error _) -> "VerifyPIN ERR"
  | Withdraw -> "Withdraw"
  | RequestEject -> "RequestEject"
;;

let transition event state =
  match (state, event) with
  | Ready, InsertCard -> ValidatingCard
  | ValidatingCard, VerifyCard res -> (
      match res with
      | Error err ->
          Printf.printf "Invalid card, eject! (err: %s)\n" err
          ; Ready
      | Ok () -> ValidatingPIN)
  | ValidatingPIN, VerifyPIN res -> (
      match res with
      | Error err ->
          Printf.printf "Invalid PIN, try again! (err: %s)\n" err
          ; ValidatingPIN
      | Ok () -> Session)
  | Session, Withdraw -> Session
  | Session, RequestEject -> Ready
  | _ ->
      Printf.printf "Invalid transition: %s cannot be run from %s\n"
        (string_of_event event) (string_of_state state)
      ; state
;;

let print_state state =
  match state with
  | Ready -> Printf.printf "ATM is ready!\n"
  | ValidatingCard -> Printf.printf "Validating card...\n"
  | ValidatingPIN -> Printf.printf "Validating PIN...\n"
  | Session -> Printf.printf "ATM session in progress.\n"
;;

let transition' event curr_state =
  let next_state = transition event curr_state in
  print_state next_state
  ; next_state
;;
