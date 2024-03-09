(*
   dune runtest -w
*)

(*
   === SUBSETS ===

If a set contains `n` elements, then the number of subsets of the set is 2^n.

So, if we have an input of ['A'; 'B'; 'C'], then the number of subsets will be:

2^3 = 8

For the subsets:
  [[]; ['A']; ['B']; ['C']; ['A'; 'B']; ['A'; 'C']; ['B'; 'C']; ['A'; 'B'; 'C']]


To visualize things, refer the file `subsets.dot`.

When we are taking the left branch, we always return the value
of our current "set", because we always append an empty list.

When we are taking the right branch, we take the whole left branch of the
current "set", and append the current value.

left,  left           -->   [] @    [] @    []           ==>   []
left,  right, left    -->   [] @ ['b'] @    []           ==>   ['b']
left,  right, right   -->   [] @ ['b'] @ ['c']           ==>   ['b'; 'c']
right, right, right   -->   [] @ ['a'] @ ['b'] @ ['c']   ==>   ['a'; 'b'; 'c']


I'm not 100% sure about the following representation:

right, right, right
  ==> List.map (List.cons 'a') @@ List.map (List.cons 'b') @@ List.map (List.cons 'c') [[]];;
  ==> [['a'; 'b'; 'c']]

right, right, left
  ==> List.map (List.cons 'a') @@ List.map (List.cons 'b') @@ [[]];;
  ==> [['a'; 'b']]
*)
let rec subsets : 'a list -> 'a list list = function
  | [] -> [ [] ]
  | h :: t ->
    let left = subsets t in
    let right = List.map (List.cons h) left in
    left @ right
;;

let%expect_test _ =
  let print f = Core.(print_s [%sexp (f : char list list)]) in
  let sort x =
    x |> List.sort compare |> List.sort (fun a b -> List.length a - List.length b)
  in
  ()
  ; print @@ subsets [ 'a'; 'b'; 'c' ]
  ; [%expect {| (() (c) (b) (b c) (a) (a c) (a b) (a b c)) |}]
  ; ()
  ; print @@ sort @@ subsets [ 'a'; 'b'; 'c' ]
  ; [%expect {| (() (a) (b) (c) (a b) (a c) (b c) (a b c)) |}]
;;

(*{|

=== PERMUTATIONS ===

Question: in how many different ways can you arrange x items?

For a list of 3 items, there are   3*2*1 = 3! =  6 permutations (aka arrangements).
For a list of 4 items, there are 4*3*2*1 = 4! = 24 permutations.

|}*)
let rec permutations = function
  | [] -> [ [] ]
  | rest ->
      List.concat_map
           (fun h -> List.map
                       (List.cons h)
                       (permutations (List.filter (fun h' -> h <> h') rest)))
           rest
[@@ocamlformat "disable"]

let%expect_test _ =
  let print lst =
    let open Core in
    print_s [%sexp (lst : int list list)]
  in
  let print' lst =
    let open Core in
    print_s [%sexp (lst : char list list)]
  in
  ()
  ; print @@ permutations []
  ; [%expect {| (()) |}]
  ; ()
  ; print @@ permutations [ 1 ]
  ; [%expect {| ((1)) |}]
  ; ()
  ; print @@ permutations [ 1; 2 ]
  ; [%expect {| ((1 2) (2 1)) |}]
  ; ()
  ; print' @@ permutations [ 'A'; 'B'; 'C' ]
  ; [%expect {| ((A B C) (A C B) (B A C) (B C A) (C A B) (C B A)) |}]
;;

(* === PERMI-COMBINATIONS === *)

let permicombs lst = List.concat_map permutations (subsets lst)

let%expect_test _ =
  let print f = Core.(print_s [%sexp (f : int list list)]) in
  let sort x =
    x |> List.sort compare |> List.sort (fun a b -> List.length a - List.length b)
  in
  ()
  ; print @@ sort @@ permicombs [ 1; 2; 3 ]
  ; [%expect
      {|
    (() (1) (2) (3) (1 2) (1 3) (2 1) (2 3) (3 1) (3 2) (1 2 3) (1 3 2) (2 1 3)
     (2 3 1) (3 1 2) (3 2 1)) |}]
;;
