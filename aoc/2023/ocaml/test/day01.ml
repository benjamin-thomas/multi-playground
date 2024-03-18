module Day01 = Aoc.Day01

let chars_of_string s = List.init (String.length s) (String.get s)

let%expect_test "filter_nums1" =
  let open Core in
  let got = Day01.Part1.filter_nums @@ chars_of_string "1abc2pqr3stu8vwx" in
  ()
  ; print_s [%sexp (got : char list)]
  ; [%expect {| (1 2 3 8) |}]
;;

let%expect_test "filter_nums2" =
  let open Core in
  let got = Day01.Part2.filter_nums @@ chars_of_string "abcone2threeightxyz" in
  ()
  ; print_s [%sexp (got : char list)]
  ; [%expect {| (1 2 3 8) |}]
;;

(*
   The newly-improved calibration document consists of lines of text; each line
   originally contained a specific calibration value that the Elves now need to recover.

   On each line, the calibration value can be found by combining the first digit and the
   last digit (in that order) to form a single two-digit number.

   For example:

   1abc2
   pqr3stu8vwx
   a1b2c3d4e5f
   treb7uchet
   In this example, the calibration values of these four lines are 12, 38, 15, and 77.
   Adding these together produces 142.
*)
let%expect_test _ =
  let open Core in
  ()
  ; print_s
      [%sexp (Day01.part1 [ "1abc2"; "pqr3stu8vwx"; "a1b2c3d4e5f"; "treb7uchet" ] : int)]
  ; [%expect {| 142 |}]
;;

let%expect_test _ =
  (*
     In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76.
     Adding these together produces 281.
  *)
  let input =
    {|

    two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen

  |}
  in
  let lines = String.split_on_char '\n' input in
  let got = Day01.part2 lines in
  let open Core in
  ()
  ; print_s [%sexp (got : int)]
  ; [%expect {| 281 |}]
;;
