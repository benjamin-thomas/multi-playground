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
  ()
  ; last [] |> Option.map print_int |> Option.value ~default:()
  ; [%expect ""]
  ; ()
  ; last [ 1 ] |> Option.map print_int |> Option.value ~default:()
  ; [%expect "1"]
  ; ()
  ; last [ 1; 2 ] |> Option.map print_int |> Option.value ~default:()
  ; [%expect "2"]
;;
