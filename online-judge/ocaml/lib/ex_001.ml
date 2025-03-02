(*
   https://oj.moonbitlang.com/problems/202412-001-find-the-unique-number

   dune exec ./test/test.exe test 001 -w
*)
let solution = List.fold_left ( lxor ) 0
