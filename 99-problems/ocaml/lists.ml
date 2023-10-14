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

(*
 * 18 - Extract a Slice From a List
 *
 * Given two indices, i and k, the slice is the list containing the elements between
 * the i'th and k'th element of the original list (both limits included).
 * Start counting the elements with 0 (this is the way the List module numbers elements).
 *)

(* My solution *)
let slice lst i k =
  let rec aux acc i k = function
    | [] -> List.rev acc
    | h :: t ->
        let acc =
          if i <= 0 && k >= 0 then
            h :: acc
          else
            acc
        in
        aux acc (i - 1) (k - 1) t
  in

  aux [] i k lst
;;

let%expect_test _ =
  let print lst = print_string @@ Show.char_list lst in
  ()
  ; print @@ slice [ 'a'; 'b'; 'c' ] 0 0
  ; [%expect "['a']"]
  ; ()
  ; print @@ slice [ 'a'; 'b'; 'c' ] 0 1
  ; [%expect {| ['a'; 'b'] |}]
  ; ()
  ; print @@ slice [ 'a'; 'b'; 'c' ] 0 2
  ; [%expect {| ['a'; 'b'; 'c'] |}]
  ; ()
  ; print @@ slice [ 'a'; 'b'; 'c' ] 0 99
  ; [%expect {| ['a'; 'b'; 'c'] |}]
  ; ()
  ; print @@ slice [ 'a'; 'b'; 'c' ] 99 0
  ; [%expect "[]"]
  ; ()
  ; print @@ slice [ 'a'; 'b'; 'c' ] 1 99
  ; [%expect "['b'; 'c']"]
  ; ()
  ; print @@ slice [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j' ] 2 6
  ; [%expect {| ['c'; 'd'; 'e'; 'f'; 'g'] |}]
;;

(* Inspired by peeking at the site's solution *)
let slice lst from until =
  let drop n lst =
    let rec aux n = function
      | [] -> []
      | _ :: t as rest ->
          if n > 0 then
            aux (n - 1) t
          else
            rest
    in
    aux n lst
  in

  let take n lst =
    let rec aux acc n = function
      | [] -> List.rev acc
      | h :: t ->
          if n > 0 then
            aux (h :: acc) (n - 1) t
          else
            List.rev acc
    in
    aux [] n lst
  in
  lst |> drop from |> take (until - from + 1)
;;

let%expect_test _ =
  let print lst = print_string @@ Show.char_list lst in
  ()
  ; print @@ slice [ 'a'; 'b'; 'c' ] 0 0
  ; [%expect "['a']"]
  ; ()
  ; print @@ slice [ 'a'; 'b'; 'c' ] 0 1
  ; [%expect {| ['a'; 'b'] |}]
  ; ()
  ; print @@ slice [ 'a'; 'b'; 'c' ] 0 2
  ; [%expect {| ['a'; 'b'; 'c'] |}]
  ; ()
  ; print @@ slice [ 'a'; 'b'; 'c' ] 0 99
  ; [%expect {| ['a'; 'b'; 'c'] |}]
  ; ()
  ; print @@ slice [ 'a'; 'b'; 'c' ] 99 0
  ; [%expect "[]"]
  ; ()
  ; print @@ slice [ 'a'; 'b'; 'c' ] 1 99
  ; [%expect "['b'; 'c']"]
  ; ()
  ; print @@ slice [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j' ] 2 6
  ; [%expect {| ['c'; 'd'; 'e'; 'f'; 'g'] |}]
;;

(*
 * 19 - Rotate a list N places to the left.
 *)

let rotate1 lst n =
  let drop n lst =
    let rec aux n = function
      | [] -> []
      | _ :: t as rest ->
          if n > 0 then
            aux (n - 1) t
          else
            rest
    in
    aux n lst
  in

  let take n lst =
    let rec aux acc n = function
      | [] -> List.rev acc
      | h :: t ->
          if n > 0 then
            aux (h :: acc) (n - 1) t
          else
            List.rev acc
    in
    aux [] n lst
  in
  let b = take n lst in
  let a = drop n lst in
  a @ b
;;

let rotate2 lst n =
  let rec aux acc n = function
    | [] -> []
    | h :: t as rest ->
        if n > 0 then
          aux (h :: acc) (n - 1) t
        else
          rest @ List.rev acc
  in
  aux [] n lst
;;

let rotate3 lst n =
  let combine a b_rev =
    let rec accum acc = function
      | [] -> acc
      | h :: t -> accum (h :: acc) t
    in
    let tmp = accum [] b_rev in
    accum tmp (List.rev a)
  in
  let rec aux acc n = function
    | [] -> []
    | h :: t as rest ->
        if n > 0 then
          aux (h :: acc) (n - 1) t
        else
          combine rest acc
  in
  aux [] n lst
;;

let%expect_test _ =
  let print lst = print_string @@ Show.char_list lst in
  ()
  ; print @@ rotate1 [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h' ] 3
  ; [%expect {| ['d'; 'e'; 'f'; 'g'; 'h'; 'a'; 'b'; 'c'] |}]
  ; ()
  ; print @@ rotate2 [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h' ] 3
  ; [%expect {| ['d'; 'e'; 'f'; 'g'; 'h'; 'a'; 'b'; 'c'] |}]
  ; ()
  ; print @@ rotate3 [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h' ] 3
  ; [%expect {| ['d'; 'e'; 'f'; 'g'; 'h'; 'a'; 'b'; 'c'] |}]
;;

(*
 * 20 - Remove the K'th element from a list.
 *)

let remove_at n lst =
  let rec aux acc curr = function
    | [] -> List.rev acc
    | h :: t ->
        let new_acc =
          if curr = n then
            acc
          else
            h :: acc
        in
        aux new_acc (curr + 1) t
  in
  aux [] 0 lst
;;

let%expect_test _ =
  let print lst = print_string @@ Show.char_list lst in
  ()
  ; print @@ remove_at 1 [ 'a'; 'b'; 'c'; 'd' ]
  ; [%expect {| ['a'; 'c'; 'd'] |}]
;;

(*
 * 21 - Insert an Element at a Given Position Into a List
 *
 * If the position is larger or equal to the length of the list, insert the element at the end.
 * (The behavior is unspecified if the position is negative.)
 *)

let insert_at x idx lst =
  let rec aux acc curr inserted = function
    | [] ->
        let new_acc =
          if inserted then
            acc
          else
            x :: acc
        in
        List.rev new_acc
    | h :: t ->
        let new_acc, inserted =
          if curr = idx then
            (h :: x :: acc, true)
          else
            (h :: acc, inserted)
        in
        aux new_acc (curr + 1) inserted t
  in
  aux [] 0 false lst
;;

let%expect_test _ =
  let print lst = print_string @@ Show.int_list lst in
  ()
  ; print @@ insert_at 0 0 [ 1; 2; 3; 4; 5 ]
  ; [%expect {| [0; 1; 2; 3; 4; 5] |}]
  ; ()
  ; print @@ insert_at 0 1 [ 1; 2; 3; 4; 5 ]
  ; [%expect {| [1; 0; 2; 3; 4; 5] |}]
  ; ()
  ; print @@ insert_at 0 2 [ 1; 2; 3; 4; 5 ]
  ; [%expect {| [1; 2; 0; 3; 4; 5] |}]
  ; ()
  ; print @@ insert_at 0 99 [ 1; 2; 3; 4; 5 ]
  ; [%expect {| [1; 2; 3; 4; 5; 0] |}]
;;

(*
 * 22 - Create a List Containing All Integers Within a Given Range
 *
 * If first argument is greater than second, produce a list in decreasing order.
 *)

let range lo hi =
  let rec aux acc curr hi =
    if curr > hi then
      acc
    else
      aux (curr :: acc) (curr + 1) hi
  in
  if lo < hi then
    aux [] lo hi |> List.rev
  else
    aux [] hi lo
;;

let%expect_test _ =
  let print lst = print_string @@ Show.int_list lst in
  ()
  ; print @@ range 4 9
  ; [%expect {| [4; 5; 6; 7; 8; 9] |}]
  ; ()
  ; print @@ range 9 4
  ; [%expect {| [9; 8; 7; 6; 5; 4] |}]
  ; ()
  ; print @@ range 5 5
  ; [%expect {| [5] |}]
;;

(*
 * 23 - Extract a Given Number of Randomly Selected Elements From a List
 *
 * The selected items shall be returned in a list.
 * We use the Random module but do not initialize it with Random.self_init for reproducibility.
 *)

let rand_select lst n =
  let len = List.length lst in
  (* NOTE: it seems tests run with a non-random seed *)
  let rand_idx () = Random.int len in
  let rec aux acc n =
    if n <= 0 then
      acc
    else
      let item = List.nth lst (rand_idx ()) in
      aux (item :: acc) (n - 1)
  in
  aux [] n
;;

let%expect_test _ =
  let print lst = print_string @@ Show.char_list lst in
  ()
  ; print @@ rand_select [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h' ] 3
  ; [%expect {| ['c'; 'g'; 'd'] |}]
;;

(*
 * 24 - Draw N different random numbers from the set 1..M.
 *)

let lotto_select n m =
  let get_rand_num () = Random.int m in
  let rec aux acc n =
    if n <= 0 then
      acc
    else
      aux (get_rand_num () :: acc) (n - 1)
  in
  aux [] n
;;

let%expect_test _ =
  let print lst = print_string @@ Show.int_list lst in
  ()
  ; print @@ lotto_select 6 49
  ; [%expect {| [17; 12; 3; 6; 21; 11] |}]
;;

(*
 * 25 - Generate a random permutation of the elements of a list.
 *
 * In other words shuffling...
 *)

(* Maybe not the best performance-wise, but readable.
 * Mmm... but actually the randomness is not good enough, see v2.
 *)
let rec permutation lst =
  match lst with
  | [] -> []
  | [ x ] -> [ x ]
  | rest ->
      let half = List.length rest / 2 in
      let a, b = split rest half in
      if Random.bool () then
        permutation a @ permutation b
      else
        permutation b @ permutation a
;;

(*
 * Even less efficient, but randomness should be better.
 *)
let permutation2 lst =
  let rec aux acc = function
    | [] -> acc
    | rest ->
        let rand_idx = Random.int (List.length rest) in
        let h = List.nth rest rand_idx in
        let t = List.filteri (fun i _ -> i <> rand_idx) rest in
        aux (h :: acc) t
  in
  aux [] lst
;;

let%expect_test _ =
  let println lst = print_endline @@ Show.int_list lst in
  ()
  ; println @@ permutation [ 1; 2; 3; 4; 5; 6 ]
  ; println @@ permutation [ 1; 2; 3; 4; 5; 6 ]
  ; println @@ permutation [ 1; 2; 3; 4; 5; 6 ]
  ; println @@ permutation [ 1; 2; 3; 4; 5; 6 ]
  ; println @@ permutation [ 1; 2; 3; 4; 5; 6 ]
  ; println @@ permutation [ 1; 2; 3; 4; 5; 6 ]
  ; println @@ permutation [ 1; 2; 3; 4; 5; 6 ]
  ; println @@ permutation [ 1; 2; 3; 4; 5; 6 ]
  ; println @@ permutation [ 1; 2; 3; 4; 5; 6 ]
  ; println @@ permutation [ 1; 2; 3; 4; 5; 6 ]
  ; [%expect
      {|
    [6; 5; 4; 2; 3; 1]
    [1; 3; 2; 5; 6; 4]
    [2; 3; 1; 4; 5; 6]
    [4; 5; 6; 2; 3; 1]
    [4; 5; 6; 1; 3; 2]
    [1; 3; 2; 4; 6; 5]
    [4; 6; 5; 1; 2; 3]
    [1; 3; 2; 4; 5; 6]
    [1; 3; 2; 5; 6; 4]
    [6; 5; 4; 2; 3; 1] |}]
  ; ()
  ; println @@ permutation2 [ 1; 2; 3; 4; 5; 6 ]
  ; println @@ permutation2 [ 1; 2; 3; 4; 5; 6 ]
  ; println @@ permutation2 [ 1; 2; 3; 4; 5; 6 ]
  ; println @@ permutation2 [ 1; 2; 3; 4; 5; 6 ]
  ; println @@ permutation2 [ 1; 2; 3; 4; 5; 6 ]
  ; println @@ permutation2 [ 1; 2; 3; 4; 5; 6 ]
  ; println @@ permutation2 [ 1; 2; 3; 4; 5; 6 ]
  ; println @@ permutation2 [ 1; 2; 3; 4; 5; 6 ]
  ; println @@ permutation2 [ 1; 2; 3; 4; 5; 6 ]
  ; println @@ permutation2 [ 1; 2; 3; 4; 5; 6 ]
  ; [%expect
      {|
    [4; 3; 6; 5; 1; 2]
    [3; 6; 1; 2; 4; 5]
    [1; 5; 6; 2; 3; 4]
    [3; 1; 2; 6; 4; 5]
    [3; 6; 5; 2; 4; 1]
    [2; 4; 1; 5; 6; 3]
    [4; 2; 3; 1; 6; 5]
    [3; 2; 4; 1; 6; 5]
    [6; 5; 4; 3; 2; 1]
    [2; 1; 3; 5; 6; 4] |}]
;;

(*
 * 26 - Generate the combinations of K distinct objects chosen from the N elements of a list.
 *
 * In how many ways can a committee of 3 be chosen from a group of 12 people?
 * We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients).
 * For pure mathematicians, this result may be great.
 * But we want to really generate all the possibilities in a list.
 *
 * C(12,3) means the number of combinations of n elements out of a group of 12, aka "n choose k".
 * It's calculated using the binomial coefficient formula:
 *   C(n, k) = n! / (k!(n - k)!)
 *
 * See: https://www.omnicalculator.com/math/binomial-coefficient?c=EUR&v=hide:1,n:12,k:3
 * See also: https://www.dcode.fr/combinations
 *)

(*
  That one was tough. I had to peak around to find a solution I was happy with.

  There's something about list comprehension (in Haskell) that simplifies the problem.
  Although that solution is too rigid to handle the dynamic param `n` (n=2 -> (i,j), n=3 -> (i,j,k), etc.)

  =============================================================================

  utop # extract 2 (range 1 4);;
  - : int list list = [[1; 2]; [1; 3]; [1; 4]; [2; 3]; [2; 4]; [3; 4]]

  utop # extract 3 (range 1 4);;
  - : int list list = [[1; 2; 3]; [1; 2; 4]; [1; 3; 4]; [2; 3; 4]]

  =============================================================================

  ghci> [ (i,j) | i <- [1..4], j <- [i..4], i /= j ]
  [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

  ghci> [ (i,j,k) | i <- [1..4], j <- [i..4], k <- [j..4], i /= j && j /= k ]
  [(1,2,3),(1,2,4),(1,3,4),(2,3,4)]

  =============================================================================
 *)
let extract n lst =
  let () = if n <= 0 then raise @@ Invalid_argument "n must be positive" in
  (* outer = [inner, inner, inner, ...] *)
  let rec aux n lst (inner : 'a list) (outer : 'a list list) : 'list list =
    if n = 0 then
      List.rev inner :: outer
    else
      match lst with
      | [] -> outer
      | x :: xs ->
          let new_outer = aux (n - 1) xs (x :: inner) outer in
          aux n xs inner new_outer
  in
  aux n lst [] [] |> List.rev
;;

let%expect_test _ =
  let print lst = print_string @@ Show.char_list_list lst in
  ()
  ; print @@ extract 1 [ 'A'; 'B'; 'C'; 'D' ]
  ; [%expect {| [['A']; ['B']; ['C']; ['D']] |}]
  ; ()
  ; ()
  ; () (* (4 choose 2 = 6 combinations) – 4!/2!(4-2)! *)
  ; print @@ extract 2 [ 'A'; 'B'; 'C'; 'D' ]
  ; [%expect
      {| [['A'; 'B']; ['A'; 'C']; ['A'; 'D']; ['B'; 'C']; ['B'; 'D']; ['C'; 'D']] |}]
  ; ()
  ; ()
  ; () (* (4 choose 3 = 4 combinations) – 4!/3!(4-3)! *)
  ; print @@ extract 3 [ 'A'; 'B'; 'C'; 'D' ]
  ; [%expect
      {| [['A'; 'B'; 'C']; ['A'; 'B'; 'D']; ['A'; 'C'; 'D']; ['B'; 'C'; 'D']] |}]
  ; ()
  ; ()
  ; () (* (4 choose 4 = 1 combination) – 4!/4!(4-4)! *)
  ; print @@ extract 4 [ 'A'; 'B'; 'C'; 'D' ]
  ; [%expect {| [['A'; 'B'; 'C'; 'D']] |}]
  ; ()
  ; ()
  ; () (* (4 choose 4 = 1 combination) – 4!/4!(4-4)! *)
  ; print @@ extract 5 [ 'A'; 'B'; 'C'; 'D' ]
  ; [%expect {| [] |}]
;;

let extract2 n lst =
  let rec aux acc rest n =
    if n = 0 then
      [ List.rev acc ]
    else
      match rest with
      | [] -> []
      | x :: xs -> aux (x :: acc) xs (n - 1) @ aux acc xs n
  in
  aux [] lst n
;;

let%expect_test _ =
  let print lst = print_string @@ Show.char_list_list lst in
  ()
  ; print @@ extract2 1 [ 'A'; 'B'; 'C'; 'D' ]
  ; [%expect {| [['A']; ['B']; ['C']; ['D']] |}]
  ; ()
  ; ()
  ; () (* (4 choose 2 = 6 combinations) – 4!/2!(4-2)! *)
  ; print @@ extract2 2 [ 'A'; 'B'; 'C'; 'D' ]
  ; [%expect
      {| [['A'; 'B']; ['A'; 'C']; ['A'; 'D']; ['B'; 'C']; ['B'; 'D']; ['C'; 'D']] |}]
  ; ()
  ; ()
  ; () (* (4 choose 3 = 4 combinations) – 4!/3!(4-3)! *)
  ; print @@ extract2 3 [ 'A'; 'B'; 'C'; 'D' ]
  ; [%expect
      {| [['A'; 'B'; 'C']; ['A'; 'B'; 'D']; ['A'; 'C'; 'D']; ['B'; 'C'; 'D']] |}]
  ; ()
  ; ()
  ; () (* (4 choose 4 = 1 combination) – 4!/4!(4-4)! *)
  ; print @@ extract2 4 [ 'A'; 'B'; 'C'; 'D' ]
  ; [%expect {| [['A'; 'B'; 'C'; 'D']] |}]
;;

(*

Given a set of n things, there are 2n combinations.
Produce the combinations for the list [a, b, c]

1. []
2. [a]
3. [b]
4. [c]
5. [a,b]
6. [a,c]
7. [b,c]
8. [a,b,c]

*)

let rec sub_sets (lst : char list) =
  match lst with
  | [] -> [ [] ]
  | h :: t ->
      let left = sub_sets t in
      let right = List.map (List.cons h) left in
      left @ right
;;

let%expect_test _ =
  let print lst = print_string @@ Show.char_list_list lst in
  ()
  ; print @@ sub_sets [ 'a'; 'b'; 'c' ]
  ; [%expect
      {| [[]; ['c']; ['b']; ['b'; 'c']; ['a']; ['a'; 'c']; ['a'; 'b']; ['a'; 'b'; 'c']] |}]
;;

(* Much easier to understand but less performant. I'll have to revisit this. *)
let extract3 n lst =
  sub_sets lst |> List.filter (fun ss -> List.length ss = n) |> List.rev
;;

let%expect_test _ =
  let print lst = print_string @@ Show.char_list_list lst in
  ()
  ; print @@ extract3 1 [ 'A'; 'B'; 'C'; 'D' ]
  ; [%expect {| [['A']; ['B']; ['C']; ['D']] |}]
  ; ()
  ; ()
  ; () (* (4 choose 2 = 6 combinations) – 4!/2!(4-2)! *)
  ; print @@ extract3 2 [ 'A'; 'B'; 'C'; 'D' ]
  ; [%expect
      {| [['A'; 'B']; ['A'; 'C']; ['A'; 'D']; ['B'; 'C']; ['B'; 'D']; ['C'; 'D']] |}]
  ; ()
  ; ()
  ; () (* (4 choose 3 = 4 combinations) – 4!/3!(4-3)! *)
  ; print @@ extract3 3 [ 'A'; 'B'; 'C'; 'D' ]
  ; [%expect
      {| [['A'; 'B'; 'C']; ['A'; 'B'; 'D']; ['A'; 'C'; 'D']; ['B'; 'C'; 'D']] |}]
  ; ()
  ; ()
  ; () (* (4 choose 4 = 1 combination) – 4!/4!(4-4)! *)
  ; print @@ extract3 4 [ 'A'; 'B'; 'C'; 'D' ]
  ; [%expect {| [['A'; 'B'; 'C'; 'D']] |}]
;;

(*
 * 27 - Group the elements of a set into disjoint subsets.
 * TODO: I'm skipping this exercise for now.
 * TODO: I haven't internalized ex. 26 yet, so trying to tackle this one now would be counter-productive.
 *)

(*

1. In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons?
   Write a function that generates all the possibilities and returns them in a list.

2. Generalize the above function in a way that we can specify a list of group sizes and the
   function will return a list of groups.

let input = group [ "a"; "b"; "c"; "d" ] [ 2; 1 ]

let output =
  [ [ [ "a"; "b" ]; [ "c" ] ]
  ; [ [ "a"; "c" ]; [ "b" ] ]
  ; [ [ "b"; "c" ]; [ "a" ] ]
  ; [ [ "a"; "b" ]; [ "d" ] ]
  ; [ [ "a"; "c" ]; [ "d" ] ]
  ; [ [ "b"; "c" ]; [ "d" ] ]
  ; [ [ "a"; "d" ]; [ "b" ] ]
  ; [ [ "b"; "d" ]; [ "a" ] ]
  ; [ [ "a"; "d" ]; [ "c" ] ]
  ; [ [ "b"; "d" ]; [ "c" ] ]
  ; [ [ "c"; "d" ]; [ "a" ] ]
  ; [ [ "c"; "d" ]; [ "b" ] ]
  ]
;;

---

Here's the LISP problem (better) explained:

a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons?
   Write a function that generates all the possibilities and returns them in a list.

Example:
* (group3 '(aldo beat carla david evi flip gary hugo ida))
( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
... )

b) Generalize the above function in a way that we can specify a list of group sizes and the function
   will return a list of groups.

Example:
* (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
... )

Note that we do not want permutations of the group members;
i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...).

However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).

You may find more about this combinatorial problem in a good book on discrete mathematics
under the term "multinomial coefficients".

 *)

(*
 * 28 - Sorting a list of lists according to length of sublists

 1. (length_sort)
    We suppose that a list contains elements that are lists themselves.
    The objective is to sort the elements of this list according to their length.
    E.g. short lists first, longer lists later, or vice versa.

 2. (frequency_sort)
    Again, we suppose that a list contains elements that are lists themselves.
    But this time the objective is to sort the elements of this list according to their
    length frequency; i.e., in the default, where sorting is done ascendingly, lists
    with rare lengths are placed first, others with a more frequent length come later.

 *)

let length_sort = List.sort (fun a b -> List.length a - List.length b)

let%expect_test _ =
  let print lst = print_string @@ Show.char_list_list lst in
  ()
  ; print
    @@ length_sort
         [ [ 'a'; 'b'; 'c' ]
         ; [ 'd'; 'e' ]
         ; [ 'f'; 'g'; 'h' ]
         ; [ 'd'; 'e' ]
         ; [ 'i'; 'j'; 'k'; 'l' ]
         ; [ 'm'; 'n' ]
         ; [ 'o' ]
         ]
  ; [%expect
      {|[['o']; ['d'; 'e']; ['d'; 'e']; ['m'; 'n']; ['a'; 'b'; 'c']; ['f'; 'g'; 'h']; ['i'; 'j'; 'k'; 'l']]|}]
;;

let group_by f lst =
  let rec aux acc f = function
    | [] -> acc
    | h :: t ->
        let new_acc =
          let n = f h in
          match List.assoc_opt n acc with
          | None -> (n, [ h ]) :: acc
          | Some t2 -> (n, h :: t2) :: List.remove_assoc n acc
        in
        aux new_acc f t
  in
  aux [] f lst
;;

let frequency_sort lst =
  lst
  |> group_by List.length
  |> List.map (fun (_, values) ->
         (List.length values, List.sort compare values))
  |> List.sort compare
  |> List.map snd
  |> List.flatten
;;

let%expect_test _ =
  (*
    Pretty easy to do in Ruby:
    lst = [%w[a b c], %w[d e], %w[f g h], %w[d e], %w[i j k l], %w[m n], %w[o]]
    lst.group_by(&:length).values.sort_by(&:length).map(&:flatten)

    Clojure:
    (def lst '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
    (->> lst (group-by count) (vals) (sort-by count) (map #(apply list %1)))

    Elixir:
    lst = [['a', 'b', 'c'], ['d', 'e'], ['f', 'g', 'h'], ['d', 'e'], ['i', 'j', 'k', 'l'], ['m', 'n'], ['o']]
    lst |> Enum.group_by(&length(&1)) |> Map.values |> Enum.sort_by(fn(a) -> {length(a), Enum.at(a, 0)} end) |> Enum.flat_map(&(&1))

    F#:
    let lst = [['a';'b';'c'];['d';'e'];['f';'g';'h'];['d';'e'];['i';'j';'k';'l'];['m';'n'];['o']];;
    lst |> List.groupBy List.length |> List.map (fun (_, values) -> values) |> List.sortBy List.length |> List.collect id;;

    Haskell: -> (??)
    lst = [["a", "b", "c"], ["d", "e"], ["f", "g", "h"], ["d", "e"], ["i", "j", "k", "l"], ["m", "n"], ["o"]]
   *)
  let print lst = print_string @@ Show.char_list_list lst in
  ()
  ; print
    @@ frequency_sort
         [ [ 'a'; 'b'; 'c' ]
         ; [ 'd'; 'e' ]
         ; [ 'f'; 'g'; 'h' ]
         ; [ 'd'; 'e' ]
         ; [ 'i'; 'j'; 'k'; 'l' ]
         ; [ 'm'; 'n' ]
         ; [ 'o' ]
         ]
  ; [%expect
      {|[['i'; 'j'; 'k'; 'l']; ['o']; ['a'; 'b'; 'c']; ['f'; 'g'; 'h']; ['d'; 'e']; ['d'; 'e']; ['m'; 'n']]|}]
;;
