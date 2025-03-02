(*
   dune exec ./test/test.exe test 001 -w
*)

module T001 = struct
  module M = Lib.Ex_001

  let solution () =
    let assert_eq = Alcotest.(check int) "" in
    assert_eq 4 @@ M.solution [ 1; 1; 2; 2; 3; 3; 4; 5; 5 ];
    assert_eq 2 @@ M.solution [ 0; 1; 0; 1; 2 ];
    assert_eq 10 @@ M.solution [ 7; 3; 3; 7; 10 ];
    ()
  ;;
end

module T003 = struct
  module M = Lib.Ex_003

  let find_offset () =
    let assert_eq = Alcotest.(check int) "" in
    assert_eq 0 @@ M.find_offset "1234";
    assert_eq 1 @@ M.find_offset "1234.";
    assert_eq 2 @@ M.find_offset "1234.2";
    assert_eq 3 @@ M.find_offset "1234.23";
    ()
  ;;

  let remove_leading_zeros () =
    let assert_eq = Alcotest.(check string) "" in
    assert_eq "1234" @@ M.remove_leading_zeros "00001234";
    assert_eq "1234" @@ M.remove_leading_zeros "1234";
    assert_eq "" @@ M.remove_leading_zeros "";
    assert_eq "wat" @@ M.remove_leading_zeros "wat";
    assert_eq "12300" @@ M.remove_leading_zeros "0012300";
    ()
  ;;

  let solution () =
    let assert_eq = Alcotest.(check string) "" in
    assert_eq "0.123" @@ M.solution "0.123";
    assert_eq "1,294,512" @@ M.solution "1294512";
    assert_eq "1,294,513.8" @@ M.solution "1294513.8";
    assert_eq "1,294,512.12412" @@ M.solution "1294512.12412";
    assert_eq "123,456,789.99" @@ M.solution "0000123456789.99";
    assert_eq "987,654,321" @@ M.solution "987654321";
    ()
  ;;
end

module T004 = struct
  module M = Lib.Ex_004

  let solution () =
    let assert_eq = Alcotest.(check int) "" in
    assert_eq 14 @@ M.solution [ "123"; "456"; "789" ];
    assert_eq 4 @@ M.solution [ "123456789" ];
    assert_eq 10 @@ M.solution [ "14329"; "7568" ];
    ()
  ;;
end

let () =
  let open Alcotest in
  run
    "Online Judge"
    [ ("Ex001", [ ("Solution", `Quick, T001.solution) ])
    ; ( "Ex003"
      , [ ("Find offset", `Quick, T003.find_offset)
        ; ("Strip zeros", `Quick, T003.remove_leading_zeros)
        ; ("Solution", `Quick, T003.solution)
        ] )
    ; ("Ex004", [ ("Solution", `Quick, T004.solution) ])
    ]
;;
