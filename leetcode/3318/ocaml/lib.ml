open Impl

module TestOccurrences = struct
  open Core

  let%expect_test _ =
    let test lst =
      print_s [%sexp (sorted_occurrences (occurrences lst) : (int * int) list)]
    in
    ()
    ; test [ 1; 1; 2; 2; 3; 4 ]
    ; [%expect {| ((2 2) (1 2) (4 1) (3 1)) |}]
    ; ()
    ; test [ 1; 2; 2; 3; 4; 2 ]
    ; [%expect {| ((2 3) (4 1) (3 1) (1 1)) |}]
    ; ()
    ; test [ 2; 2; 3; 4; 2; 3 ]
    ; [%expect {| ((2 3) (3 2) (4 1)) |}]
  ;;

  let%expect_test "sum2" =
    let test lst =
      print_s [%sexp (sum_occurences 2 @@ sorted_occurrences @@ occurrences lst : int)]
    in
    ()
    ; test [ 1; 1; 2; 2; 3; 4 ]
    ; [%expect {| 6 |}]
    ; ()
    ; test [ 1; 2; 2; 3; 4; 2 ]
    ; [%expect {| 10 |}]
    ; ()
    ; test [ 2; 2; 3; 4; 2; 3 ]
    ; [%expect {| 12 |}]
    ; ()
  ;;
end

module TestWindow = struct
  open Core

  let%expect_test "drop" =
    ()
    ; print_s [%sexp (drop 2 [ 1; 2; 3; 4; 5 ] : int list)]
    ; [%expect {| (3 4 5) |}]
    ; ()
    ; print_s [%sexp (drop 0 [ 1; 2; 3; 4; 5 ] : int list)]
    ; [%expect {| (1 2 3 4 5) |}]
    ; ()
    ; ()
    ; print_s [%sexp (drop 99 [ 1; 2; 3 ] : int list)]
    ; [%expect {| () |}]
  ;;

  let%expect_test "take_exactly" =
    ()
    ; print_s [%sexp (take 3 [ 1; 2; 3; 4 ] : int list)]
    ; [%expect {| (1 2 3) |}]
    ; print_s [%sexp (take 3 [ 4; 5 ] : int list)]
    ; [%expect {| () |}]
    ; ()
  ;;

  let%expect_test _ =
    ()
    ; print_s [%sexp (make_windows 2 [ 1; 2; 3; 4; 5 ] : int list list)]
    ; [%expect {| ((1 2) (2 3) (3 4) (4 5)) |}]
    ; print_s [%sexp (make_windows 3 [ 1; 2; 3; 4; 5 ] : int list list)]
    ; [%expect {| ((1 2 3) (2 3 4) (3 4 5)) |}]
  ;;
end

module TestImpl = struct
  open Core

  let%expect_test "example1" =
    ()
    ; print_s [%sexp (solve [ 1; 1; 2; 2; 3; 4; 2; 3 ] 6 2 : int)]
    ; [%expect {| 28 |}]
  ;;

  let%expect_test "example2" =
    ()
    ; print_s [%sexp (solve [ 3; 8; 7; 8; 7; 5 ] 2 2 : int)]
    ; [%expect {| 68 |}]
  ;;
end
