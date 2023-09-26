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

(*
  > This solution can also be written in terms of a fold.

  - http://community.schemewiki.org/?ninety-nine-scheme-problems
  - http://community.schemewiki.org/?S-99-01
 *)
let last' lst = List.fold_left (fun _acc n -> Some n) None lst

let%expect_test _ =
  let print int_opt =
    int_opt |> Option.map print_int |> Option.value ~default:()
  in
  ()
  ; print @@ last []
  ; [%expect ""]
  ; ()
  ; print @@ last' []
  ; [%expect ""]
  ; ()
  ; print @@ last [ 1 ]
  ; [%expect "1"]
  ; ()
  ; print @@ last' [ 1 ]
  ; [%expect "1"]
  ; ()
  ; print @@ last [ 1; 2 ]
  ; [%expect "2"]
  ; ()
  ; print @@ last' [ 1; 2 ]
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

(*
 * 7 - Flatten a nested list structure.
 *)

type 'a node = One of 'a | Many of 'a node list

(* My solution *)
let flatten (lst : 'a node list) =
  let rec loop acc = function
    | [] -> acc
    | One h :: t -> loop (h :: acc) t
    | Many l :: r -> rev (loop acc l) @ rev (loop [] r)
  in

  loop [] lst |> rev
;;

(* The site's solution - better! *)
let flatten' (lst : 'a node list) =
  let rec loop acc = function
    | [] -> acc
    | One x :: t -> loop (x :: acc) t
    | Many l :: r -> loop (loop acc l) r
  in
  List.rev (loop [] lst)
;;

let%expect_test _ =
  let print lst = print_string @@ String.concat ", " lst in
  let x =
    [ One "a"
    ; Many
      [ One "b"
      ; Many
        [ One "c"
        ; One "d"
        ]
      ; One "e"
      ; One "f"
      ]
    ] in
  ()
  ; print @@ flatten x
  ; [%expect {| a, b, c, d, e, f |}]
  ; print @@ flatten' x
  ; [%expect {| a, b, c, d, e, f |}]
[@@ocamlformat "disable"]

(*
 * 8 - Eliminate consecutive duplicates of list elements.
 *)

(* My solution *)
let compress lst =
  let rec loop acc seen = function
    | [] -> acc
    | h :: t ->
        if h = seen then
          loop acc seen t
        else
          loop (h :: acc) h t
  in

  match lst with
  | [] -> []
  | h :: t -> List.rev (loop [ h ] h t)
;;

(* The site's solution 🤯️ *)
let rec compress' = function
  | a :: (b :: _ as rest) ->
      if a = b then
        compress' rest
      else
        a :: compress' rest
  | smaller -> smaller
;;

(* Chat GPT! *)
let compress'' lst =
  let rec loop acc = function
    | [] -> List.rev acc
    | a :: (b :: _ as rest) when a = b -> loop acc rest
    | a :: rest -> loop (a :: acc) rest
  in
  loop [] lst
;;

let%expect_test _ =
  let print lst = print_string @@ Show.string_list lst in
  let input = [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ] in
  let expect () = [%expect{| ["a"; "b"; "c"; "a"; "d"; "e"] |}] in
  ()
  ; print @@ compress input
  ; expect ()
  ; print @@ compress' input
  ; expect ()
  ; print @@ compress'' input
  ; expect ()
[@@ocamlformat "disable"]

(*
 * 9. Pack consecutive duplicates of list elements into sublists.
 *)

(* My solution *)
let pack lst =
  let rec loop acc = function
    | a :: (b :: _rest as rest) ->
        if a = b then
          loop (a :: acc) rest
        else
          (a :: acc) :: loop [] rest
    | smaller -> [ acc @ smaller ]
  in
  loop [] lst
;;

(* The site's solution, better. *)
let pack' lst =
  let rec loop curr acc = function
    | [] -> [] (* original list is empty *)
    | [ x ] -> (x :: curr) :: acc
    | a :: (b :: _ as t) ->
        if a = b then
          loop (a :: curr) acc t
        else
          loop [] ((a :: curr) :: acc) t
  in
  List.rev (loop [] [] lst)
;;

(* Chat GPT! *)
let pack'' lst =
  let rec loop acc curr = function
    | [] -> List.rev (curr :: acc)
    | h :: t -> (
        match curr with
        | [] -> loop acc [ h ] t
        | h' :: _ when h = h' -> loop acc (h :: curr) t
        | _ -> loop (curr :: acc) [ h ] t)
  in
  match lst with
  | [] -> []
  | h :: t -> loop [] [ h ] t
;;

let%expect_test _ =
  let to_lst_fmt items =
    "[" ^ String.concat "; " items ^ "]"
  in
  let deep1 lst = to_lst_fmt @@ List.map (sprintf {|"%s"|}) lst in
  let deep2 lst = to_lst_fmt @@ List.map deep1 lst in
  let print lst = print_string @@ deep2 lst in
  let expect () = [%expect {| [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]] |}] in
  ()
  ; print @@ pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]
  ; expect ()
  ; print @@ pack' ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]
  ; expect ()
  ; print @@ pack'' ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]
  ; expect ()
[@@ocamlformat "disable"]

(*
 * 10 - Run-Length Encoding
 *
 * Use the result of problem P09 to implement the so-called run-length encoding data compression method.
 * Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
 *)

let encode lst =
  let rec loop acc ((cnt, h') as curr) = function
    | [] -> List.rev (curr :: acc)
    | h :: t ->
        if h = h' then
          loop acc (cnt + 1, h') t
        else
          loop (curr :: acc) (1, h) t
  in
  match lst with
  | [] -> []
  | h :: t -> loop [] (1, h) t
;;

(* Imperative style *)
let encode' lst =
  if lst = [] then
    []
  else
    let curr = ref (1, List.hd lst) in
    let acc = ref [] in
    for i = 1 to List.length lst - 1 do
      let cnt, x = !curr in
      let x' = List.nth lst i in
      ()
      ; if x = x' then (
          ()
          ; curr := (cnt + 1, x)
        ) else (
          ()
          ; acc := !acc @ [ !curr ]
          ; curr := (1, x')
        )
    done
    ; !acc @ [ !curr ]
;;

let%expect_test _ =
  let string_of_items lst =
    let f (x, y) = sprintf {|(%d, '%c')|} x y in
    let x = String.concat "; " (List.map f lst) in
    "[" ^ x ^ "]"
  in
  let print lst = print_string @@ string_of_items lst in
  let input = ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e'] in
  let expect () = [%expect {|[(4, 'a'); (1, 'b'); (2, 'c'); (2, 'a'); (1, 'd'); (4, 'e')]|}] in
  ()
  ; print @@ encode []
  ; [%expect "[]"]
  ; print @@ encode ['A']
  ; [%expect {|[(1, 'A')]|}]
  ; print @@ encode ['A'; 'A']
  ; [%expect {|[(2, 'A')]|}]
  ; print @@ encode input
  ; expect ()
  ; print @@ encode' []
  ; [%expect "[]"]
  ; print @@ encode' ['A']
  ; [%expect {|[(1, 'A')]|}]
  ; print @@ encode' input
  ; expect ()
[@@ocamlformat "disable"]

(*
 * 11 - Modified Run-Length Encoding
 *
 * Modify the result of the previous problem in such a way that if an element has no
 * duplicates it is simply copied into the result list.
 *
 * Only elements with duplicates are transferred as (N E) lists.
 *)

type 'a rle = One of 'a | Many of (int * 'a)

let encode2 lst =
  let rec loop curr acc = function
    | [] -> curr :: acc
    | h :: t -> (
        match curr with
        | One h2 ->
            if h = h2 then
              loop (Many (2, h)) acc t
            else
              loop (One h) (curr :: acc) t
        | Many (n, h2) ->
            if h = h2 then
              loop (Many (n + 1, h)) acc t
            else
              loop (One h) (curr :: acc) t)
  in
  match lst with
  | [] -> []
  | h :: t -> loop (One h) [] t |> List.rev
;;

let%expect_test _ =
  let item = function
    | One a -> sprintf "One '%c'" a
    | Many (a, b) -> sprintf "Many (%d, '%c')" a b
  in
  let items lst = "["^ String.concat "; " (List.map item lst)  ^"]" in
  let input = ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e'] in
  let expect () =
    [%expect {| [Many (4, 'a'); One 'b'; Many (2, 'c'); Many (2, 'a'); One 'd'; Many (4, 'e')] |}]
  in
  ()
  ; print_string @@ items @@ encode2 []
  ; [%expect {| [] |}]
  ; print_string @@ items @@ encode2 ['a']
  ; [%expect {| [One 'a'] |}]
  ; print_string @@ items @@ encode2 ['a'; 'a']
  ; [%expect {| [Many (2, 'a')] |}]
  ; print_string @@ items @@ encode2 ['a'; 'a'; 'a']
  ; [%expect {| [Many (3, 'a')] |}]
  ; print_string @@ items @@ encode2 ['a'; 'a'; 'a'; 'b']
  ; [%expect {| [Many (3, 'a'); One 'b'] |}]
  ; print_string @@ items @@ encode2 input
  ; expect ()
[@@ocamlformat "disable"]

(*
 * 12 - Decode a Run-Length Encoded List
 *
 * Given a run-length code list generated as specified in the previous problem, construct its uncompressed version.
 *)

let decode lst =
  let rec loop acc = function
    | [] -> acc
    | h :: t -> (
        match h with
        | One x -> loop (x :: acc) t
        | Many (a, b) ->
            let () = assert (a > 0) in
            if a = 1 then
              loop (b :: acc) t
            else
              loop (b :: acc) (Many (a - 1, b) :: t))
  in
  loop [] lst |> List.rev
;;

let repeat x =
  let rec many acc x = function
    | 0 -> acc
    | n -> many (x :: acc) x (n - 1)
  in
  many [] x
;;

let%expect_test _ =
  let print lst = print_string @@ Show.char_list lst in
  ()
  ; print @@ repeat 'a' 3
  ; [%expect {| ['a'; 'a'; 'a'] |}]
;;

(* Adapted from the site's solution *)
let decode' lst =
  let rec many acc x = function
    | 0 -> acc
    | n -> many (x :: acc) x (n - 1)
  in
  let rec loop acc = function
    | [] -> acc
    | One x :: t -> loop (x :: acc) t
    | Many (n, x) :: t -> loop (many acc x n) t
  in

  loop [] lst |> List.rev
;;

let%expect_test _ =
let print lst = print_string @@ Show.char_list lst in
  let input = [Many (4, 'a'); One 'b'; Many (2, 'c'); Many (2, 'a'); One 'd'; Many (4, 'e')] in
  let expect () = [%expect {| ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e'] |}] in
  ()
  ; print @@ decode []
  ; [%expect {| [] |}]
  ; print @@ decode [ One 'a' ]
  ; [%expect {| ['a'] |}]
  ; print @@ decode [ One 'a'; One 'b' ]
  ; [%expect {| ['a'; 'b'] |}]
  ; print @@ decode [ Many (2, 'a') ]
  ; [%expect {| ['a'; 'a'] |}]
  ; print @@ decode [ Many (2, 'a'); One 'b' ]
  ; [%expect {| ['a'; 'a'; 'b'] |}]
  ; print @@ decode input
  ; expect ()
  ; print @@ decode' input
  ; expect ()
[@@ocamlformat "disable"]

(*
 * 13 - Run-Length Encoding of a List (Direct Solution)
 *
 * Skip: there seems to be a mistake on the website.
 * I see no difference with the prior exercises (even looking at the answers).
 *)

(*
 * 14 - Duplicate the Elements of a List
 *)

let duplicate lst =
  let rec loop acc = function
    | [] -> acc
    | h :: t -> loop (h :: h :: acc) t
  in
  loop [] lst |> List.rev
;;

let%expect_test _ =
  let print lst = print_string @@ Show.char_list lst in
  ()
  ; print @@ duplicate []
  ; [%expect {| [] |}]
  ; print @@ duplicate [ 'a' ]
  ; [%expect {| ['a'; 'a'] |}]
  ; print @@ duplicate [ 'a'; 'b'; 'c'; 'c'; 'd' ]
  ; [%expect {| ['a'; 'a'; 'b'; 'b'; 'c'; 'c'; 'c'; 'c'; 'd'; 'd'] |}]
;;

(*
 * 15 - Replicate the Elements of a List a Given Number of Times
 *)

(* First attempt *)
let replicate_orig lst n =
  let rec aux acc pattern n =
    if n <= 0 then
      acc
    else
      aux (acc @ pattern) pattern (n - 1)
  in
  aux [] lst n
;;

(* 2nd attempt, after taking a peak at the site's solution --> remove the list append operation *)
let replicate lst n =
  let prepend acc lst =
    let rec aux acc = function
      | [] -> acc
      | h :: t -> aux (h :: acc) t
    in
    aux acc (List.rev lst)
  in
  let rec aux acc pattern n =
    if n <= 0 then
      acc
    else
      aux (prepend acc pattern) pattern (n - 1)
  in
  aux [] lst n
;;

let%expect_test _ =
  let print lst = print_string @@ Show.char_list lst in
  ()
  ; print @@ replicate [ 'a'; 'b'; 'c' ] 0
  ; [%expect {| [] |}]
  ; ()
  ; print @@ replicate [ 'a'; 'b'; 'c' ] 1
  ; [%expect {| ['a'; 'b'; 'c'] |}]
  ; ()
  ; print @@ replicate [ 'a'; 'b'; 'c' ] 2
  ; [%expect {| ['a'; 'b'; 'c'; 'a'; 'b'; 'c'] |}]
  ; ()
  ; print @@ replicate [ 'a'; 'b'; 'c' ] 3
  ; [%expect {| ['a'; 'b'; 'c'; 'a'; 'b'; 'c'; 'a'; 'b'; 'c'] |}]
;;

(*
 * 16 - Drop every N'th element from a list.
 *)

let drop lst n =
  let rec aux acc n' = function
    | [] -> List.rev acc
    | h :: t ->
        if n' = 1 then
          aux acc n t
        else
          aux (h :: acc) (n' - 1) t
  in
  aux [] n lst
;;

let%expect_test _ =
  let print lst = print_string @@ Show.char_list lst in
  ()
  ; print @@ drop [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j' ] 3
  ; [%expect {| ['a'; 'b'; 'd'; 'e'; 'g'; 'h'; 'j'] |}]
  ; ()
  ; print @@ drop [] 3
  ; [%expect {| [] |}]
  ; ()
  ; print @@ drop [ 'a'; 'b'; 'c' ] 0
  ; [%expect {| ['a'; 'b'; 'c'] |}]
  ; ()
  ; print @@ drop [ 'a'; 'b'; 'c' ] 1
  ; [%expect {| [] |}]
  ; ()
  ; print @@ drop [ 'a'; 'b'; 'c' ] 2
  ; [%expect {| ['a'; 'c'] |}]
;;

(*
 * 17 - Split a list into two parts; the length of the first part is given.
 *
 * If the length of the first part is longer than the entire list, then the
 * first part is the list and the second part is empty.
 *)

let split lst n =
  let rec aux (a, b) rest n =
    match rest with
    | [] -> (List.rev a, b)
    | h :: t ->
        if n > 0 then
          aux (h :: a, []) t (n - 1)
        else
          (List.rev a, h :: t)
  in
  aux ([], []) lst n
;;

let%expect_test _ =
  let print (a, b) = print_string @@ Show.(tup2 (char_list a, char_list b)) in
  ()
  ; print @@ split [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j' ] 3
  ; [%expect {| (['a'; 'b'; 'c'], ['d'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j']) |}]
  ; ()
  ; print @@ split [ 'a'; 'b'; 'c'; 'd' ] 5
  ; [%expect {| (['a'; 'b'; 'c'; 'd'], []) |}]
;;
