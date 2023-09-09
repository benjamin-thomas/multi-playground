(*

dune runtest -w

dune utop
> utop # Ninety_nine_problems.Lists.last [];;
- : 'a option = None

*)

open Printf

(*
 * 1 - Write a function last : 'a list -> 'a option that returns the last element of a list
 *)
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

(*
 * 2 - Find the last but one (last and penultimate) elements of a list.
 *)
let rec last_two lst : ('a * 'b) option =
  match lst with
  | [] -> None
  | [ a; b ] -> Some (a, b)
  | _ :: t -> last_two t
;;

let%expect_test _ =
  let print = function
    | None -> printf "None"
    | Some (a, b) -> printf {|Some ("%s", "%s")|} a b
  in
  ()
  ; print @@ last_two [ "a"; "b"; "c"; "d" ]
  ; [%expect {|Some ("c", "d")|}]
  ; print @@ last_two [ "a" ]
  ; [%expect {| None |}]
  ; print @@ last_two []
  ; [%expect {| None |}]
;;

(*
 * 3 - Find the N'th element of a list.
 *)

let nth lst n =
  let rec loop n lst =
    match (n, lst) with
    | 0, h :: _ -> h
    | _, [] -> failwith "nth"
    | _, _ :: t -> loop (n - 1) t
  in
  loop n lst
;;

let%expect_test _ =
  ()
  ; print_string @@ nth [ "a"; "b"; "c"; "d"; "e" ] 2
  ; [%expect {| c |}]
  ; print_string
      (try nth [ "a" ] 2 with
      | _ -> "FAILED")
  ; [%expect "FAILED"]
;;

let rec at lst n =
  match lst with
  | [] -> None
  | h :: t ->
      if n > 0 then
        at t (n - 1)
      else
        Some h
;;

let%expect_test _ =
  let print = function
    | None -> print_string "None"
    | Some x -> printf {|Some "%s"|} x
  in
  ()
  ; print @@ at [ "a"; "b"; "c"; "d"; "e" ] 2
  ; [%expect {| Some "c" |}]
  ; print @@ at [ "a" ] 2
  ; [%expect "None"]
;;

(*
 * 4 - Find the number of elements of a list.
 *)

let len lst =
  let rec loop n = function
    | [] -> n
    | _ :: t -> loop (n + 1) t
  in
  loop 0 lst
;;

let%expect_test _ =
  ()
  ; print_int @@ len []
  ; [%expect {| 0 |}]
  ; print_int @@ len [ 'A' ]
  ; [%expect {| 1 |}]
  ; print_int @@ len [ 'A'; 'B' ]
  ; [%expect {| 2 |}]
;;

(*
 * 5 - Reverse a list.
 *)

let rev lst =
  let rec loop acc = function
    | [] -> acc
    | h :: t -> loop (h :: acc) t
  in
  loop [] lst
;;

let%expect_test _ =
  let print lst =
    print_string @@ String.concat ", " (List.map string_of_int lst)
  in
  ()
  ; print @@ rev []
  ; [%expect]
  ; print @@ rev [ 1 ]
  ; [%expect {| 1 |}]
  ; print @@ rev [ 1; 2; 3 ]
  ; [%expect {| 3, 2, 1 |}]
;;

(*
 * 6 - Find out whether a list is a palindrome.
 *)

let is_palindrome lst = lst = rev lst
