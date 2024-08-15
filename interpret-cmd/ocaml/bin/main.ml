(*
   dune exec ./bin/main.exe --no-print-directory --display=quiet -w
*)

module Program = Lib.Program

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

let () =
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
