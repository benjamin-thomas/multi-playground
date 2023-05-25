(* Use the compiler
 *   echo monads.ml | entr -c bash -c 'ocamlc ./monads.ml && ./a.out'
 *)

(* Or use the top-level interactive mode
 * echo monads.ml | entr -c ocaml /_
 *)

let add_a a b c =
  match a with
  | None -> None
  | Some x -> (
      match b with
      | None -> None
      | Some y -> (
          match c with
          | None -> None
          | Some z -> Some (x + y + z)))
;;

let add_b a b c =
  Option.bind a (fun x ->
    Option.bind b (fun y ->
      Option.bind c (fun z ->
        Some (x + y + z)
      )
    )
  )
  [@@ocamlformat "disable"]

let add_c a b c =
  match (a, b, c) with
  | Some x, Some y, Some z -> Some (x + y + z)
  | _ -> None
;;

let ( let* ) = Option.bind

let add_d a b c =
  let* x = a in
  let* y = b in
  let* z = c in
  Some (x + y + z)
;;

let ( >>= ) opt f =
  match opt with
  | Some x -> f x
  | None -> None
;;

let add_e a b c =
  a >>= fun x ->
  b >>= fun y ->
  c >>= fun z ->
  Some(x + y +z )
  [@@ocamlformat "disable"]

(*
 See the Elm blog post at: https://thoughtbot.com/blog/running-out-of-maps

 Rather than using `and_map`, I'll use the function name `apply` (it seems fit?)
 *)
let apply opt_a opt_b =
  match (opt_a, opt_b) with
  | Some value, Some fn -> Some (fn value)
  | _, _ -> None
;;

let add_f a b c =
  Some (fun x y z -> x + y + z)
    |> apply a
    |> apply b
    |> apply c
  [@@ocamlformat "disable"]

let apply res_a res_b =
  match (res_a, res_b) with
  | Ok value, Ok fn -> Ok (fn value)
  | Error err, _ -> Error err
  | _, Error err -> Error err
;;

let add_g a b c =
  Ok (fun x y z -> x + y + z)
    |> apply a
    |> apply b
    |> apply c
  [@@ocamlformat "disable"]

(* See again the Elm examples *)

let apply' res_a res_b =
  Result.bind res_b (fun partial -> Result.map partial res_a)
;;

let add_h a b c =
  Ok (fun x y z -> x + y + z)
   |> apply' a
   |> apply' b
   |> apply' c
  [@@ocamlformat "disable"]

let and_then a b = Result.bind b a

let add_i a b c =
   Ok (fun x y z -> x + y + z)
     |> and_then (fun partial -> Result.map partial a)
     |> and_then (fun partial -> Result.map partial b)
     |> and_then (fun partial -> Result.map partial c)
   [@@ocamlformat "disable"]

(* MAIN *)

let printRes header fn =
  match fn with
  | None -> Printf.printf "%s => None\n" header
  | Some n -> Printf.printf "%s => Some %d\n" header n
;;

let printRes2 header fn =
  match fn with
  | Error s -> Printf.printf "%s => Error \"%s\"\n" header s
  | Ok n -> Printf.printf "%s => Ok %d\n" header n
;;

let _ =
  (* add_a: verbose! *)
  printRes "add_a (Some 1) (Some 2) (Some 3)"
  @@ add_a (Some 1) (Some 2) (Some 3)
  ; printRes "add_a None (Some 2) (Some 3)    " @@ add_a None (Some 2) (Some 3)
  ; printRes "add_a (Some 1) None (Some 3)    " @@ add_a (Some 1) None (Some 3)
  ; printRes "add_a (Some 1) None None        " @@ add_a (Some 1) None (Some 3)
  ; print_endline ""
  ; (* add_b: fn chain *)
    printRes "add_b (Some 1) (Some 2) (Some 3)"
    @@ add_b (Some 1) (Some 2) (Some 3)
  ; printRes "add_b None (Some 2) (Some 3)    " @@ add_b None (Some 2) (Some 3)
  ; printRes "add_b (Some 1) None (Some 3)    " @@ add_b (Some 1) None (Some 3)
  ; printRes "add_b (Some 1) None None        " @@ add_b (Some 1) None (Some 3)
  ; print_endline ""
  ; (* add_c: use pattern matching *)
    printRes "add_c (Some 1) (Some 2) (Some 3)"
    @@ add_c (Some 1) (Some 2) (Some 3)
  ; printRes "add_c None (Some 2) (Some 3)    " @@ add_c None (Some 2) (Some 3)
  ; printRes "add_c (Some 1) None (Some 3)    " @@ add_c (Some 1) None (Some 3)
  ; printRes "add_c (Some 1) None None        " @@ add_c (Some 1) None (Some 3)
  ; print_endline ""
  ; (* add_d: use let bind *)
    printRes "add_d (Some 1) (Some 2) (Some 3)"
    @@ add_d (Some 1) (Some 2) (Some 3)
  ; printRes "add_d None (Some 2) (Some 3)    " @@ add_d None (Some 2) (Some 3)
  ; printRes "add_d (Some 1) None (Some 3)    " @@ add_d (Some 1) None (Some 3)
  ; printRes "add_d (Some 1) None None        " @@ add_d (Some 1) None (Some 3)
  ; print_endline ""
  ; (* add_e: use bind operator, Haskell style *)
    printRes "add_e (Some 1) (Some 2) (Some 3)"
    @@ add_e (Some 1) (Some 2) (Some 3)
  ; printRes "add_e None (Some 2) (Some 3)    " @@ add_e None (Some 2) (Some 3)
  ; printRes "add_e (Some 1) None (Some 3)    " @@ add_e (Some 1) None (Some 3)
  ; printRes "add_e (Some 1) None None        " @@ add_e (Some 1) None (Some 3)
  ; print_endline ""
  ; (* add_f: Elm style *)
    printRes "add_f (Some 1) (Some 2) (Some 3)"
    @@ add_f (Some 1) (Some 2) (Some 3)
  ; printRes "add_f None (Some 2) (Some 3)    " @@ add_f None (Some 2) (Some 3)
  ; printRes "add_f (Some 1) None (Some 3)    " @@ add_f (Some 1) None (Some 3)
  ; printRes "add_f (Some 1) None None        " @@ add_f (Some 1) None (Some 3)
  ; print_endline ""
  ; (* add_g: Elm style. NOTE: as with Elm, the arguments are processed in reverse. *)
    printRes2 "add_g (Ok 1) (Ok 2) (Ok 3)                  " @@ add_g (Ok 1) (Ok 2) (Ok 3)
  ; printRes2 "add_g (Error 'Oops1') (Ok 2) (Ok 3)         " @@ add_g (Error "Oops1") (Ok 2) (Ok 3)
  ; printRes2 "add_g (Ok 1) (Error 'Oops2') (Ok 3)         " @@ add_g (Ok 1) (Error "Oops2") (Ok 3)
  ; printRes2 "add_g (Ok 1) (Error 'Oops2') (Error 'Oops3')" @@ add_g (Ok 1) (Error "Oops2") (Error "Oops3")
  ; print_endline ""
  ; (* add_h: Elm style again. This fixes the arguments being processed in reverse order. *)
    printRes2 "add_h (Ok 1) (Ok 2) (Ok 3)                  " @@ add_h (Ok 1) (Ok 2) (Ok 3)
  ; printRes2 "add_h (Error 'Oops1') (Ok 2) (Ok 3)         " @@ add_h (Error "Oops1") (Ok 2) (Ok 3)
  ; printRes2 "add_h (Ok 1) (Error 'Oops2') (Ok 3)         " @@ add_h (Ok 1) (Error "Oops2") (Ok 3)
  ; printRes2 "add_h (Ok 1) (Error 'Oops2') (Error 'Oops3')" @@ add_h (Ok 1) (Error "Oops2") (Error "Oops3")
  ; print_endline ""
  (* add_h: Elm style again, but verbose mode. It processes the arguments being processed in the correct order.
   * Note that I had to create a custom Result.bind (and_then) with a reversed order to make that work.
   *)
  ; printRes2 "add_i (Ok 1) (Ok 2) (Ok 3)                  " @@ add_i (Ok 1) (Ok 2) (Ok 3)
  ; printRes2 "add_i (Error 'Oops1') (Ok 2) (Ok 3)         " @@ add_i (Error "Oops1") (Ok 2) (Ok 3)
  ; printRes2 "add_i (Ok 1) (Error 'Oops2') (Ok 3)         " @@ add_i (Ok 1) (Error "Oops2") (Ok 3)
  ; printRes2 "add_i (Ok 1) (Error 'Oops2') (Error 'Oops3')" @@ add_i (Ok 1) (Error "Oops2") (Error "Oops3")
  ; print_endline ""
[@@ocamlformat "disable"]

let succeed b =
  if b then
    Ok 1
  else
    Error "Oops!"
;;

let ( let* ) = Result.bind

let compute_ok_ok =
  let* a = succeed true in
  let* b = succeed true in
  Ok (a + b)
;;

let compute_ok_err =
  let* a = succeed true in
  let* b = succeed false in
  Ok (a + b)
;;

let print_my_result = function
  | Error s -> s
  | Ok n -> string_of_int n
;;

(** returns either Ok, otherwise the Error of the same type
 *  This is somewhat unusual I suppose, I just want to print the original error below.
 *  I could also use the `print_result` function above.
 *)
let ok_or_err r =
  match r with
  | Ok v -> v
  | Error e -> e
;;

let () =
  ()
  ; Printf.printf "a) Add with (Ok, Ok): %s\n%!"
      (compute_ok_ok
      |> Result.map (fun n -> string_of_int n)
      |> Result.value ~default:"oops")
  ; Printf.printf "a) Add with (Ok, Err): %s\n%!"
      (compute_ok_err
      |> Result.map (fun n -> string_of_int n)
      |> Result.value ~default:"oops")
  ; Printf.printf "b) Add with (Ok, Ok): %s\n%!"
      (compute_ok_ok |> Result.map (fun n -> string_of_int n) |> ok_or_err)
  ; Printf.printf "b) Add with (Ok, Err): %s\n%!"
      (compute_ok_err |> Result.map (fun n -> string_of_int n) |> ok_or_err)
  ; Printf.printf "c) Add with (Ok, Ok): %s\n%!"
      (compute_ok_ok |> print_my_result)
  ; Printf.printf "c) Add with (Ok, Err): %s\n%!"
      (compute_ok_err |> print_my_result)
;;
