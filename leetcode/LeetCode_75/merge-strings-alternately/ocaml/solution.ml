(* dune runtest -w *)

let char_list_of_string str = List.init (String.length str) (String.get str)

let merge word1 word2 =
  let xs = char_list_of_string word1 in
  let ys = char_list_of_string word2 in
  let rec aux acc = function
    | ([], []) -> acc
    | ([], h2 :: t2) -> aux (h2 :: acc) ([], t2)
    | (h1 :: t1, []) -> aux (h1 :: acc) (t1, [])
    | (h1 :: t1, h2 :: t2) -> aux (h1 :: h2 :: acc) (t1, t2)
  in
  let combined = aux [] (ys, xs) in
  let buf = Buffer.create 16 in
  ()
  ; List.iter (Buffer.add_char buf) (List.rev combined)
  ; Buffer.contents buf
;;

let merge_imperative word1 word2 =
  let buf = Buffer.create 16 in
  let a_len = String.length word1 in
  let b_len = String.length word2 in
  for i = 0 to max a_len b_len - 1 do
    ()
    ; if i < a_len then Buffer.add_char buf word1.[i]
    ; if i < b_len then Buffer.add_char buf word2.[i]
  done
  ; Buffer.contents buf
;;

let%expect_test _ =
  ()
  ; print_string @@ merge "ABC" "abc"
  ; [%expect {| AaBbCc |}]
  ; print_string @@ merge "AB" "abcde"
  ; [%expect {| AaBbcde |}]
  ; print_string @@ merge "ABCDE" "ab"
  ; [%expect {| AaBbCDE |}]
  ; print_string @@ merge "abc" "pqr"
  ; [%expect {|apbqcr|}]
  ; ()
  ; print_string @@ merge "ab" "pqrs"
  ; [%expect {|apbqrs|}]
;;

let%expect_test _ =
  ()
  ; print_string @@ merge_imperative "ABC" "abc"
  ; [%expect {| AaBbCc |}]
  ; print_string @@ merge_imperative "AB" "abcde"
  ; [%expect {| AaBbcde |}]
  ; print_string @@ merge_imperative "ABCDE" "ab"
  ; [%expect {| AaBbCDE |}]
  ; print_string @@ merge_imperative "abc" "pqr"
  ; [%expect {|apbqcr|}]
  ; ()
  ; print_string @@ merge_imperative "ab" "pqrs"
  ; [%expect {|apbqrs|}]
;;
