(*
   dune exec ./bin/main.exe --no-print-directory --display=quiet -w
*)

module State = struct
  type ('s, 'a) t = 's -> 'a * 's

  let return x s = x, s

  let bind m f s =
    let x, s' = m s in
    f x s'
  ;;

  let get s = s, s
  let modify f s = (), f s
end

module Program = struct
  (* Cmd GADT *)
  type _ cmd =
    | Invite : string -> unit cmd
    | Here : string list cmd
    | Status : string -> unit cmd
    | Bind : 'a cmd * ('a -> 'b cmd) -> 'b cmd

  module Cmd = struct
    let bind x f = Bind (x, f)
  end

  type app_state =
    { started_at : float
    ; users : string list
    ; counter : int
    }

  let pp_app_state fmt state =
    let started_at_str = Printf.sprintf "%.0f" state.started_at in
    let users_str = String.concat ", " state.users in
    Format.fprintf
      fmt
      "@[<v>App State:@, Started at: %s@, Users: [%s]@, Counter: %d@]"
      started_at_str
      users_str
      state.counter
  ;;

  let rec interpret : type a. a cmd -> (app_state, a) State.t = function
    | Bind (f, g) -> State.bind (interpret f) (fun x -> interpret (g x))
    | Invite user_name ->
      State.modify (fun state ->
        let new_state =
          { state with users = user_name :: state.users; counter = state.counter + 1 }
        in
        print_endline ("You invited @" ^ user_name ^ "!");
        new_state)
    | Here ->
      [%do
        begin
          state <- State.get;
          let users = state.users in
          let () =
            print_endline
              ("In this channel: "
               ^ String.concat " " users
               ^ " (counter="
               ^ string_of_int state.counter
               ^ ")")
          in
          State.return users
        end [@monad State]]
    | Status descr ->
      [%do
        begin
          state <- State.get;
          let counter = state.counter + 10 in
          let () =
            print_endline
              ("Setting status to '"
               ^ descr
               ^ "' (old_counter="
               ^ string_of_int state.counter
               ^ ", new_counter="
               ^ string_of_int counter
               ^ ")")
          in
          State.modify (fun state -> { state with counter });
          State.return ()
        end [@monad State]]
  ;;
end

module App = struct
  let init : float -> Program.app_state =
    fun curr_time ->
    { started_at = curr_time; users = [ "Marge"; "Homer"; "Maggie" ]; counter = 0 }
  ;;

  let actions =
    [%do
      begin
        users <- Here;
        Invite "Lisa";
        users <- Here;
        Invite "Bart";
        Status "Working from home";
        Status "Going to sleep"
      end [@monad Program.Cmd]]
  ;;

  let print_header color header =
    let color =
      match color with
      | `YELLOW -> 33
      | `BLUE -> 34
      | `PURPLE -> 35
    in
    Printf.printf "\x1b[1;%dm=== %s ===\x1b[0m\n" color header
  ;;

  let print_state color header state =
    let print st =
      Program.pp_app_state Format.str_formatter st;
      print_endline @@ Format.flush_str_formatter ()
    in
    print_header color ("BEG:" ^ header);
    print state;
    print_header color ("END:" ^ header)
  ;;

  let start () =
    let state = init (Unix.time ()) in
    print_state `YELLOW "START" state;
    print_newline ();
    print_header `PURPLE "BEG:RUNNING ";
    let (), final_state = Program.interpret actions state in
    print_header `PURPLE "END:RUNNING";
    print_newline ();
    print_state `BLUE "FINISH" final_state;
    ()
  ;;
end

module Tests = struct
  let green s = Printf.printf "\x1b[32m%s\x1b[0m\n" s

  let test_invite_user () =
    (* Inving a user adds the user to the list and increments the counter by 1 *)
    let (), got =
      Program.(interpret (Invite "Lisa"))
        { started_at = 123.0; users = [ "Bob" ]; counter = 0 }
    in
    let want : Program.app_state =
      { started_at = 123.0; users = [ "Lisa"; "Bob" ]; counter = 1 }
    in
    assert (want = got);
    green "=> Invite user test passed!"
  ;;

  let test_here_command () =
    (* Here command returns the list of users *)
    let got, _ =
      Program.(interpret Here)
        { started_at = 123.0; users = [ "Marge"; "Homer" ]; counter = 0 }
    in
    let want = [ "Marge"; "Homer" ] in
    assert (want = got);
    green "=> Here command test passed!"
  ;;

  let test_status_command () =
    (* Setting the status oddly increments the counter by 10 *)
    let (), got =
      Program.(interpret (Status "Working from home"))
        { started_at = 123.0; users = [ "Marge" ]; counter = 0 }
    in
    let want : Program.app_state =
      { started_at = 123.0; users = [ "Marge" ]; counter = 10 }
    in
    assert (got = want);
    green "=> Status command test passed!"
  ;;

  let test_sequence_of_commands () =
    let _, got =
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
    assert (got = want);
    green "=> Sequence of commands test passed!"
  ;;

  let start () =
    let test_functions =
      [ test_invite_user
      ; test_here_command
      ; test_status_command
      ; test_sequence_of_commands
      ]
    in
    List.iter (fun test -> test ()) test_functions
  ;;
end

let () =
  let testing = true in
  if not testing then
    App.start ()
  else
    Tests.start ()
;;
