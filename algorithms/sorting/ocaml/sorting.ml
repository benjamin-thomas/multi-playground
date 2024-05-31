open Core

let rec merge a b =
  match (a, b) with
  | ([], []) -> []
  | ([], h :: t) -> h :: merge [] t
  | (h :: t, []) -> h :: merge t []
  | (h1 :: t1, h2 :: t2) ->
    if h1 < h2 then
      h1 :: merge t1 (h2 :: t2)
    else
      h2 :: merge (h1 :: t1) t2
;;

let%expect_test "Merge 2 sorted lists" =
  let test f = print_s [%sexp (f : int list)] in
  ()
  ; test @@ merge [] []
  ; [%expect {| () |}]
  ; ()
  ; test @@ merge [ 2; 5; 6 ] [ 1; 3; 4 ]
  ; [%expect {| (1 2 3 4 5 6) |}]
;;

let halve lst =
  let rec aux (l, r) = function
    | [] -> (l, r)
    | [ x ] -> (x :: l, r)
    | hl :: hr :: t -> aux (hl :: l, hr :: r) t
  in
  aux ([], []) lst
;;

let%expect_test "Split a list into 2 halves whose length differs by at most one" =
  let test f = print_s [%sexp (f : int list * int list)] in
  ()
  ; test @@ halve []
  ; [%expect {| (() ()) |}]
  ; ()
  ; test @@ halve [ 1 ]
  ; [%expect {| ((1) ()) |}]
  ; test @@ halve [ 1; 2 ]
  ; [%expect {| ((1) (2)) |}]
  ; test @@ halve [ 1; 2; 3 ]
  ; [%expect {| ((3 1) (2)) |}]
  ; test @@ halve [ 1; 2; 3; 4 ]
  ; [%expect {| ((3 1) (4 2)) |}]
  ; test @@ halve [ 1; 2; 3; 4; 5 ]
  ; [%expect {| ((5 3 1) (4 2)) |}]
  ; test @@ halve [ 1; 2; 3; 4; 5; 6 ]
  ; [%expect {| ((5 3 1) (6 4 2)) |}]
;;

let rec msort = function
  | [] -> []
  | [ x ] -> [ x ]
  | xs ->
    let (l, r) = halve xs in
    merge (msort l) (msort r)
;;

let%expect_test "Now use merge and halve to implement merge sort" =
  let test f = print_s [%sexp (f : int list)] in
  ()
  ; test @@ msort []
  ; [%expect {| () |}]
  ; ()
  ; test @@ msort [ 1; 4; 3; 6; 5; 4; 8; 2; 3; 1; 9 ]
  ; [%expect {| (1 1 2 3 3 4 4 5 6 8 9) |}]
;;
