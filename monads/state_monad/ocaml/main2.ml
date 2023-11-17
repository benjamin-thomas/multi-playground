(*
   dune exec ./main2.exe
*)

(* Force new line when using dune watch *)
let () = print_newline ()
let () = print_newline ()

(*
 * RANDOM IMPLEMENTATION
 *)

(* Temporary tracing functions, just for debugging purposes. *)
let trace fmt = Printf.printf (fmt ^^ "\n%!")
let trace' fmt = trace (fmt ^^ "\n")

let random seed =
  let () = trace "Calling random" in
  let a = 1103515245 in
  let c = 12345 in
  let m = 2147483648 in
  ((a * seed) + c) mod m
;;

(*
 * MANUAL STATE HANDLING
 *)

let () =
  let a = random 0 in
  let b = random a in
  let c = random b in
  trace' "Manual state handling: (%d, %d, %d)" a b c
;;

(*
 * INTERNAL STATE HANDLING
 *)

let gen_rand =
  let seed = ref 0 in
  fun () ->
    ()
    ; seed := random !seed
    ; !seed
;;

let () =
  let a = gen_rand () in
  let b = gen_rand () in
  let c = gen_rand () in
  trace' "Internal state handling: (%d, %d, %d)" a b c
;;

(*
 * USING THE STATE MONAD
 *
 * Fundamental qualities of the state monad:
 *   - no mutable state was used
 *   - all functions invoved are pure
 *   - the functions are composable (see [Random.map])
 *)

module Random = Preface.State.Over (struct
    type t = int
  end)

module Program : sig
  val start : int -> int * int * int
  val start2 : int -> int * bool * int
  val start3 : int -> bool * bool * bool
end = struct
  open Random.Syntax

  let rand_num : int Random.t =
    let* seed = Random.get in
    let new_seed = random seed in
    let* () = Random.set new_seed in
    Random.return new_seed
  ;;

  let rand_bool = Random.map (fun n -> n mod 2 = 0) rand_num

  let rand_tup3 : (int * int * int) Random.t =
    let* a = rand_num in
    let* b = rand_num in
    let* c = rand_num in
    Random.return (a, b, c)
  ;;

  let rand_tup3' : (int * bool * int) Random.t =
    let* a = rand_num in
    let* b = rand_bool in
    let* c = rand_num in
    Random.return (a, b, c)
  ;;

  let rand_tup3'' : (bool * bool * bool) Random.t =
    let* a = rand_bool in
    let* b = rand_bool in
    let* c = rand_bool in
    Random.return (a, b, c)
  ;;

  let start = Random.eval_identity rand_tup3
  let start2 = Random.eval_identity rand_tup3'
  let start3 = Random.eval_identity rand_tup3''
end

let () =
  let (a, b, c) = Program.start 0 in
  let (d, e, f) = Program.start2 0 in
  let (g, h, i) = Program.start3 0 in
  ()
  ; trace "Using the state monad: (%d, %d, %d)" a b c
  ; trace "Using the state monad: (%d, %b, %d)" d e f
  ; trace "Using the state monad: (%b, %b, %b)" g h i
;;
