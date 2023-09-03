(*

dune runtest -w

dune utop
> utop # Ninety_nine_problems.Lists.last [];;
- : 'a option = None

*)

(* Write a function last : 'a list -> 'a option that returns the last element of a list *)
let rec last = function
  | [] -> None
  | x :: [] -> Some x
  | _ :: xs -> last xs
;;

let%expect_test _ =
  let print int_opt =
    int_opt |> Option.map print_int |> Option.value ~default:()
  in
  ()
  ; print @@ last []
  ; [%expect ""]
  ; ()
  ; print @@ last [ 1 ]
  ; [%expect "1"]
  ; ()
  ; print @@ last [ 1; 2 ]
  ; [%expect "2"]
;;
