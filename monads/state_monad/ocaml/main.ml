(*
   dune exec ./main.exe
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
 * STATE MONAD HANDLING
 *)

(** Fundamental properties of the state monad:
    - no mutable state was used
    - all functions invoved in the computation are pure / no side-effect
    - should be composable *)
module State_monad = struct
  let return a state = (a, state)

  let bind fx g state =
    let (x, state) = fx state in
    let (x, state) = g x state in
    (x, state)
  ;;
end

(* State change definitions:
   - define the current state on the left
   - define the next state on the right *)

let get state = (state, ())
let put state () = ((), state)

let rand_num =
  let open State_monad in
  let ( let* ) = bind in
  let* seed = get in
  let new_seed = random seed in
  let* () = put new_seed in
  return new_seed
;;

(* Run the computation *)

let rand_tup3 =
  let open State_monad in
  bind rand_num (fun a ->
    bind rand_num (fun b ->
      bind rand_num (fun c ->
        return (a, b, c)
      )
    )
  )
[@@ocamlformat "disable"]

let rand_tup3' =
  let open State_monad in
  let ( let* ) = bind in
  let* a = rand_num in
  let* b = rand_num in
  let* c = rand_num in
  return (a, b, c)
;;

let run_state m ~init = m init
let eval_state m ~init = fst @@ m init

let () =
  let (a, b, c) = fst @@ run_state rand_tup3 ~init:0 in
  trace' "State monad (verbose): (%d, %d, %d)" a b c
;;

let () =
  let (a, b, c) = eval_state rand_tup3' ~init:0 in
  trace' "State monad (let syntax): (%d, %d, %d)" a b c
;;

let () =
  (* We don't really need run_state/eval_state like in Haskell here. *)
  let (a, b, c) = fst @@ rand_tup3 0 in
  trace' "State monad (bypassing run_state/eval_state): (%d, %d, %d)" a b c
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
  trace "Internal state handling: (%d, %d, %d)" a b c
;;
