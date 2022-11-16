(* echo monads.ml | entr -c bash -c 'ocamlc ./monads.ml && ./a.out' *)
(* echo monads.ml | entr -c ocaml /_ *)

let add_a x y z =
  match x with
  | None -> None
  | Some x_ ->
    (match y with
     | None -> None
     | Some y_ ->
       (match z with
        | None -> None
        | Some z_ -> Some (x_ + y_ + z_)))
;;

let add_b x y z =
  Option.bind x (fun x_ ->
    Option.bind y (fun y_ ->
      Option.bind z (fun z_ ->
        Some (x_ + y_ + z_)
      )
    )
  )
    [@@ocamlformat "disable"]

let add_c x y z =
  match x, y, z with
  | Some x, Some y, Some z -> Some (x + y + z)
  | _ -> None
;;

let ( let* ) = Option.bind

let add_d x y z =
  let* x = x in
  let* y = y in
  let* z = z in
  Some (x + y + z)
;;

let ( >>= ) opt f =
  match opt with
  | Some x -> f x
  | None -> None
;;

let add_e x y z =
  x >>= fun x ->
  y >>= fun y ->
  z >>= fun z ->
  Some(x + y +z )
  [@@ocamlformat "disable"]

(* MAIN *)

let printRes header fn =
  match fn with
  | None -> Printf.printf "%s => None\n" header
  | Some n -> Printf.printf "%s => Some %d\n" header n
;;

let _ =
  (* add_a: verbose! *)
  printRes "add_a (Some 1) (Some 2) (Some 3)" @@ add_a (Some 1) (Some 2) (Some 3);
  printRes "add_a None (Some 2) (Some 3)    " @@ add_a None (Some 2) (Some 3);
  printRes "add_a (Some 1) None (Some 3)    " @@ add_a (Some 1) None (Some 3);
  printRes "add_a (Some 1) None None        " @@ add_a (Some 1) None (Some 3);
  print_endline "";
  (* add_b: fn chain *)
  printRes "add_b (Some 1) (Some 2) (Some 3)" @@ add_b (Some 1) (Some 2) (Some 3);
  printRes "add_b None (Some 2) (Some 3)    " @@ add_b None (Some 2) (Some 3);
  printRes "add_b (Some 1) None (Some 3)    " @@ add_b (Some 1) None (Some 3);
  printRes "add_b (Some 1) None None        " @@ add_b (Some 1) None (Some 3);
  print_endline "";
  (* add_c: use pattern matching *)
  printRes "add_c (Some 1) (Some 2) (Some 3)" @@ add_c (Some 1) (Some 2) (Some 3);
  printRes "add_c None (Some 2) (Some 3)    " @@ add_c None (Some 2) (Some 3);
  printRes "add_c (Some 1) None (Some 3)    " @@ add_c (Some 1) None (Some 3);
  printRes "add_c (Some 1) None None        " @@ add_c (Some 1) None (Some 3);
  print_endline "";
  (* add_d: use let bind *)
  printRes "add_d (Some 1) (Some 2) (Some 3)" @@ add_d (Some 1) (Some 2) (Some 3);
  printRes "add_d None (Some 2) (Some 3)    " @@ add_d None (Some 2) (Some 3);
  printRes "add_d (Some 1) None (Some 3)    " @@ add_d (Some 1) None (Some 3);
  printRes "add_d (Some 1) None None        " @@ add_d (Some 1) None (Some 3);
  print_endline "";
  (* add_e: use bind operator, Haskell style *)
  printRes "add_e (Some 1) (Some 2) (Some 3)" @@ add_e (Some 1) (Some 2) (Some 3);
  printRes "add_e None (Some 2) (Some 3)    " @@ add_e None (Some 2) (Some 3);
  printRes "add_e (Some 1) None (Some 3)    " @@ add_e (Some 1) None (Some 3);
  printRes "add_e (Some 1) None None        " @@ add_e (Some 1) None (Some 3);
  print_endline ""
;;
