(*
   dune test -w
*)

module Program = Lib.Program

let add x y = x + y

let test_addition () =
  Alcotest.(check int) "1+2=3" 3 (add 1 2);
  Alcotest.(check int) "2+3=5" 5 (add 2 3);
  ()
;;

let app_state_testable : Program.app_state Alcotest.testable =
  Alcotest.testable Program.pp_app_state ( = )
;;

let test_invite_user () =
  let (), got =
    Program.(interpret (Invite "Lisa"))
      { started_at = 123.0; users = [ "Bob" ]; counter = 0 }
  in
  let want : Program.app_state =
    { started_at = 123.0; users = [ "Lisa"; "Bob" ]; counter = 1 }
  in
  Alcotest.check app_state_testable "Invite user test" want got
;;

let test_here_command () =
  let got, _ =
    Program.(interpret Here)
      { started_at = 123.0; users = [ "Marge"; "Homer" ]; counter = 0 }
  in
  let want = [ "Marge"; "Homer" ] in
  Alcotest.(check (list string)) "Here command test" want got
;;

let test_status_command () =
  let (), got =
    Program.(interpret (Status "Working from home"))
      { started_at = 123.0; users = [ "Marge" ]; counter = 0 }
  in
  let want : Program.app_state =
    { started_at = 123.0; users = [ "Marge" ]; counter = 10 }
  in
  Alcotest.check app_state_testable "Status command test" want got
;;

let test_sequence_of_commands () =
  let (), got =
    let actions =
      [%do
        begin
          Status "About to start...";
          Invite "Lisa";
          Invite "Bart";
          Status "Working from home"
        end [@monad Program.Cmd]]
    in
    Program.interpret actions { started_at = 234.0; users = [ "Marge" ]; counter = 0 }
  in
  let want : Program.app_state =
    { started_at = 234.0; users = [ "Bart"; "Lisa"; "Marge" ]; counter = 22 }
  in
  Alcotest.check app_state_testable "Sequence of commands test" want got
;;

let () =
  Alcotest.(
    run
      "Program"
      [ ( "interpret"
        , List.map
            (fun (name, f) -> test_case name `Quick f)
            [ "test_addition", test_addition
            ; "test_invite_user", test_invite_user
            ; "test_here_command", test_here_command
            ; "test_status_command", test_status_command
            ; "test_sequence_of_commands", test_sequence_of_commands
            ] )
      ])
;;
