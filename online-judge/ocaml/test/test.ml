(*
   dune exec ./test/test.exe test 001 -w
*)

let others () =
  let e = epsilon_float in
  ();
  Alcotest.(check @@ float e) "0 is 0" 0. 0.;
  Alcotest.(check @@ float e) "0 is epsilon" 0. e;
  Alcotest.(check @@ neg @@ float e) "0 is not 1" 0. 1.;
  Alcotest.(check @@ neg @@ float e) "1 is not 0" 1. 0.;
  Alcotest.(check @@ float e) ".3 is .3" (0.1 +. 0.2) 0.3
;;

module Ex_001 = Lib.Ex_001

let ex_001_tests () =
  let assert_eq = Alcotest.(check int) "" in
  ();
  assert_eq 4 @@ Ex_001.solution [ 1; 1; 2; 2; 3; 3; 4; 5; 5 ];
  assert_eq 2 @@ Ex_001.solution [ 0; 1; 0; 1; 2 ];
  assert_eq 10 @@ Ex_001.solution [ 7; 3; 3; 7; 10 ]
;;

module Ex_003 = Lib.Ex_003

let ex_003_tests () =
  (*{|

  inspect!(solution("1294512.12412"), content="1,294,512.12412")
  inspect!(solution("0000123456789.99"), content="123,456,789.99")
  inspect!(solution("987654321"), content="987,654,321")

  |}*)
  let assert_eq = Alcotest.(check string) "" in
  assert_eq "1,294,512" @@ Ex_003.solution "1294512";
  assert_eq "1,294,513.8" @@ Ex_003.solution "1294513.8";
  ()
;;

(* ; assert_eq "1,294,512.12412" @@ Ex_003.solution "1294512.12412" *)

(* ; assert_eq "123,456,789.99" @@ Ex_003.solution "0000123456789.99" *)
(* ; assert_eq "987,654,321" @@ Ex_003.solution "987654321" *)

let () =
  let open Alcotest in
  run
    "Online Judge"
    [ ("Ex001", [ ("Suite", `Quick, ex_001_tests) ])
    ; ("Ex003", [ ("Suite", `Quick, ex_003_tests) ])
    ; ("Other floats", [ ("others", `Quick, others) ])
    ]
;;
