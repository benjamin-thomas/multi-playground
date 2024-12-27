open Printf

(*
   Find the source of a crash with:

OCAMLRUNPARAM=b=1 dune exec ./day06b.exe
*)

module Ansi = struct
  let yellow = "\x1b[1;33m"
  let gray = "\x1b[1;90m"
  let reset = "\x1b[0m"
  let clear_screen = "\x1b[H\x1b[J"
end

type advance_mode =
  | KeyPress
  | SleepMs of int ref

type grid = char array array

let make_grid input : char array array =
  let lines = String.split_on_char '\n' (String.trim input) in
  let dim = List.length lines in
  let grid = Array.make_matrix dim dim '.' in
  List.iteri (fun y line -> String.iteri (fun x c -> grid.(x).(y) <- c) line) lines;
  grid
;;

exception Found_guard of (int * int)

let find_guard_pos_exn grid =
  try
    let dim = Array.length grid in
    for y = 0 to dim - 1 do
      for x = 0 to dim - 1 do
        if grid.(x).(y) = '^' then raise (Found_guard (x, y))
      done
    done;
    failwith "ABNORMAL: no guard on this grid"
  with
  | Found_guard p -> p
;;

let print_grid grid =
  let dim = Array.length grid in
  for y = 0 to dim - 1 do
    for x = 0 to dim - 1 do
      match grid.(x).(y) with
      | ('^' | 'v' | '>' | '<') as c -> printf "%s%c%s" Ansi.yellow c Ansi.reset
      | 'X' -> printf "%s%c%s" Ansi.gray 'X' Ansi.reset
      | c -> print_char c
    done;
    print_newline ()
  done
;;

let add_tup2 (a, b) (c, d) = (a + c, b + d)

let read_one_char () =
  (* spell:disable *)
  let open Unix in
  let term_io = tcgetattr stdin in
  let () =
    tcsetattr
      stdin
      TCSADRAIN
      { term_io with
        c_icanon = false (* Turn off canonical mode (line buffering) *)
      ; c_echo = false (* Don't echo the character *)
      ; c_vmin = 1 (* Wait for at least one character *)
      ; c_vtime = 0 (* No timeout *)
      }
  in
  let char = input_char (Unix.in_channel_of_descr Unix.stdin) in
  (* Restore terminal to original state *)
  tcsetattr stdin TCSADRAIN term_io;
  (* spell:enable *)
  char
;;

let rotate_guard = function
  | (0, -1) -> (1, 0)
  | (1, 0) -> (0, 1)
  | (0, 1) -> (-1, 0)
  | (-1, 0) -> (0, -1)
  | _ -> failwith "Invalid direction"
;;

let guard_icon = function
  | (0, -1) -> '^'
  | (1, 0) -> '>'
  | (0, 1) -> 'v'
  | (-1, 0) -> '<'
  | _ -> failwith "Invalid direction"
;;

exception Loop_detected
exception Grid_exit

let visited : ((int * int) * (int * int), unit) Hashtbl.t = Hashtbl.create 16

let print_visited () =
  Hashtbl.iter
    (fun (pos, dir) _ ->
       printf "((%d, %d), (%d, %d))" (fst pos) (snd pos) (fst dir) (snd dir))
    visited
;;

let check_bounds_exn grid x = if x < 0 || x >= Array.length grid then raise Grid_exit

let update_grid ~check_loops (guard_pos, guard_dir) grid =
  let (candidate_x, candidate_y) = add_tup2 !guard_pos !guard_dir in
  check_bounds_exn grid candidate_x;
  check_bounds_exn grid candidate_y;
  let (guard_x, guard_y) = !guard_pos in
  begin
    match grid.(candidate_x).(candidate_y) with
    | '#' | 'O' -> begin
      if check_loops
      then begin
        if Hashtbl.mem visited (!guard_pos, !guard_dir)
        then raise Loop_detected
        else Hashtbl.add visited (!guard_pos, !guard_dir) ()
      end;
      guard_dir := rotate_guard !guard_dir;
      grid.(guard_x).(guard_y) <- guard_icon !guard_dir;
      ()
    end
    | _ -> begin
      grid.(guard_x).(guard_y) <- 'X';
      guard_pos := (candidate_x, candidate_y);
      let (guard_x, guard_y) = !guard_pos in
      grid.(guard_x).(guard_y) <- guard_icon !guard_dir
    end
  end
;;

let count_trail (grid : grid) =
  Array.fold_left
    (fun acc row ->
       Array.fold_left (fun acc c -> if c = 'X' then acc + 1 else acc) acc row)
    0
    grid
;;

let trails grid =
  let positions = ref [] in
  Array.iteri
    (fun x col ->
       Array.iteri (fun y c -> if c = 'X' then positions := (x, y) :: !positions) col)
    grid;
  !positions
;;

let handle_input ms =
  (* spell:disable *)
  try
    let term_io = Unix.tcgetattr Unix.stdin in
    Unix.tcsetattr
      Unix.stdin
      Unix.TCSANOW
      { term_io with Unix.c_icanon = false; c_echo = false; c_vmin = 0; c_vtime = 0 };
    let buf = Bytes.create 1 in
    match Unix.read Unix.stdin buf 0 1 with
    | 0 -> ()
    | _ ->
      begin
        match Bytes.get buf 0 with
        | '+' -> ms := max 1 @@ (!ms - !ms)
        | '-' -> ms := !ms + !ms
        | _ -> ()
      end;
      Unix.tcsetattr Unix.stdin Unix.TCSANOW term_io
    (* spell:enable *)
  with
  | _ -> ()
;;

let game_loop ~check_loops advance_mode (iteration, loops, guard_pos, guard_dir) grid =
  let cycles = ref 0 in
  try
    while true do
      cycles := !cycles + 1;
      match advance_mode with
      | Some mode -> begin
        print_string Ansi.clear_screen;
        printf "Iteration: %d\n%!" iteration;
        printf "Cycles: %d\n%!" !cycles;
        printf "Loops: %d\n%!" loops;
        printf "Guard pos: (%d,%d)\n%!" (fst !guard_pos) (snd !guard_pos);
        print_newline ();
        printf "Visited:\n%!";
        print_visited ();
        print_newline ();
        print_newline ();
        print_grid grid;
        match mode with
        | KeyPress -> begin
          let v = ref 0 in
          incr v;
          printf "\nPress a key to advance the grid state (v=%d)\n%!" !v;
          let _ = read_one_char () in
          update_grid ~check_loops (guard_pos, guard_dir) grid;
          print_string Ansi.clear_screen;
          print_grid grid
        end
        | SleepMs ms -> begin
          printf "\nPlay speed: %dms (Press + to speed up, - to slow down)\n%!" !ms;
          handle_input ms;
          Unix.sleepf (float_of_int !ms /. 1000.0);
          update_grid ~check_loops (guard_pos, guard_dir) grid;
          print_string Ansi.clear_screen;
          print_grid grid
        end
      end
      | None -> update_grid ~check_loops (guard_pos, guard_dir) grid
    done
  with
  | Grid_exit ->
    grid.(fst !guard_pos).(snd !guard_pos) <- 'X';
    (match advance_mode with
     | None -> ()
     | Some _ -> begin
       print_string Ansi.clear_screen;
       print_grid grid
     end)
;;

let[@warning "-32"] part1 input =
  let[@warning "-26"] advance_mode = Some (SleepMs (ref 20)) in
  let[@warning "-26"] advance_mode = Some KeyPress in
  let[@warning "-26"] advance_mode = None in
  let grid = make_grid input in
  let guard_pos = ref (find_guard_pos_exn grid) in
  let guard_dir = ref (0, -1) in
  game_loop ~check_loops:false advance_mode (0, 0, guard_pos, guard_dir) grid;
  (*
     Part 1 example: Trail count: 41
     Part 1 real   : Trail count: 5404
  *)
  printf "Trail count: %d\n" (count_trail grid);
  ()
;;

(*
   Part 2 solution studied from: https://github.com/Tchou/aoc/blob/4216449/solutions/aoc2024/s06.ml

   [ocaml] $ dune exec ./day06.exe
   Trail count: 5404
*)
let[@warning "-32"] part2 input =
  let[@warning "-26"] advance_mode = Some (SleepMs (ref 30)) in
  let[@warning "-26"] advance_mode = Some KeyPress in
  let[@warning "-26"] advance_mode = None in
  let x_trails =
    let grid =
      let grid = make_grid input in
      let guard_pos = ref (find_guard_pos_exn grid) in
      let guard_dir = ref (0, -1) in
      game_loop ~check_loops:false None (0, 0, guard_pos, guard_dir) grid;
      grid
    in
    (* print_endline "Trails:"; *)
    (* print_trails @@ trails grid; *)
    (* if true then failwith "wip"; *)
    trails grid
  in
  let loops = ref 0 in
  List.iteri
    (fun i trail_pos ->
       try
         begin
           let grid = make_grid input in
           let guard_pos = ref (find_guard_pos_exn grid) in
           let guard_dir = ref (0, -1) in
           grid.(fst trail_pos).(snd trail_pos) <- 'O';
           (* printf "Iteration: %d\n%!" i; *)
           Hashtbl.clear visited;
           game_loop ~check_loops:true advance_mode (i, !loops, guard_pos, guard_dir) grid
         end
       with
       | Loop_detected -> incr loops)
    x_trails;
  printf "Loops count: %d\n" !loops
;;

(*
   Grid based on a hash table : 15s
   Grid based on an array: 800ms!

   Part 2 example: Loops count: 6
   Part 2 real   : Loops count: 1984
*)
let () =
  In_channel.with_open_text "../_inputs/06.txt"
  @@ fun ic -> In_channel.input_all ic |> part1
;;
