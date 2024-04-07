module Day03 = Aoc.Day03

let example : string =
  {|

467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..

 |}
;;

let%test_unit _ =
  let result = Day03.table example in
  let expect =
    [ [ '4'; '6'; '7'; '.'; '.'; '1'; '1'; '4'; '.'; '.' ]
    ; [ '.'; '.'; '.'; '*'; '.'; '.'; '.'; '.'; '.'; '.' ]
    ; [ '.'; '.'; '3'; '5'; '.'; '.'; '6'; '3'; '3'; '.' ]
    ; [ '.'; '.'; '.'; '.'; '.'; '.'; '#'; '.'; '.'; '.' ]
    ; [ '6'; '1'; '7'; '*'; '.'; '.'; '.'; '.'; '.'; '.' ]
    ; [ '.'; '.'; '.'; '.'; '.'; '+'; '.'; '5'; '8'; '.' ]
    ; [ '.'; '.'; '5'; '9'; '2'; '.'; '.'; '.'; '.'; '.' ]
    ; [ '.'; '.'; '.'; '.'; '.'; '.'; '7'; '5'; '5'; '.' ]
    ; [ '.'; '.'; '.'; '$'; '.'; '*'; '.'; '.'; '.'; '.' ]
    ; [ '.'; '6'; '6'; '4'; '.'; '5'; '9'; '8'; '.'; '.' ]
    ]
  in
  let open Core in
  [%test_result: char list list] ~expect result
;;

let%expect_test _ =
  let open Core in
  let get (x, y) = Day03.neighbours ~x ~y ~width:10 ~height:10 in
  ()
  ; print_s [%sexp (get (0, 0) : (int * int) list)]
  ; [%expect {| ((1 0) (0 1) (1 1)) |}]
  ; ()
  ; print_s [%sexp (get (9, 9) : (int * int) list)]
  ; [%expect {| ((8 8) (9 8) (8 9)) |}]
  ; ()
  ; print_s [%sexp (get (99, 99) : (int * int) list)]
  ; [%expect {| () |}]
;;

let%expect_test _ =
  let matrix = Day03.(to_matrix @@ table example) in
  let width = Array.length matrix in
  let height = Array.length matrix.(0) in
  let open Core in
  let get (x, y) = Day03.neighbouring_parts ~x ~y ~width ~height matrix in
  ()
  ; print_s [%sexp (get (0, 0) : char list)]
  ; [%expect {| (6 . .) |}]
  ; ()
  ; print_s [%sexp (get (9, 9) : char list)]
  ; [%expect {| (. . .) |}]
  ; ()
  ; print_s [%sexp (get (99, 99) : char list)]
  ; [%expect {| () |}]
  ; ()
  ; print_s [%sexp (get (7, 3) : char list)]
  ; [%expect {| (6 3 3 # . . . .) |}]
;;

let%test_unit _ =
  let open Core in
  (* let matrix = Day03.(to_matrix @@ table example) in *)
  (* let _width = Array.length matrix in *)
  (* let _height = Array.length matrix.(0) in *)
  ()
  ; [%test_result: int] ~expect:4361 Day03.(sum_of_parts (table example))
;;
