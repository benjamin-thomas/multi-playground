(* dune runtest -w *)

let to_chars str = List.init (String.length str) (String.get str)

let merge word1 word2 =
  let xs = to_chars word1 in
  let ys = to_chars word2 in
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
[@@warning "-32"]
;;

let merge_imperative word_a word_b =
  let buf = Buffer.create 16 in
  let len_a = String.length word_a in
  let len_b = String.length word_b in
  let i = ref 0 in
  let append = Buffer.add_char buf in
  while !i < len_a || !i < len_b do
    ()
    ; if !i < len_a then append word_a.[!i]
    ; if !i < len_b then append word_b.[!i]
    ; incr i
  done
  ; Buffer.contents buf
;;

let interleave_all lst =
  let rec aux acc = function
    | ([], []) -> List.rev acc
    | ([], h2 :: t2) -> aux (h2 :: acc) ([], t2)
    | (h1 :: t1, []) -> aux (h1 :: acc) (t1, [])
    | (h1 :: t1, h2 :: t2) -> aux (h2 :: h1 :: acc) (t1, t2)
  in
  aux [] lst
;;

let%expect_test _ =
  let print lst = List.iter (fun c -> Printf.printf "%c" c) lst in
  ()
  ; print @@ interleave_all ([ 'A'; 'C' ], [ 'B'; 'D'; 'E'; 'F' ])
  ; [%expect {| ABCDEF |}]
  ; ()
  ; print @@ interleave_all ([ 'A'; 'C'; 'E'; 'F' ], [ 'B'; 'D' ])
  ; [%expect {| ABCDEF |}]
;;

(** Same as the original version above, except I've extracted out the notion of
    interleaving into [interleave_all] *)
let merge word1 word2 =
  let interleaved = interleave_all (to_chars word1, to_chars word2) in
  let buf = Buffer.create 16 in
  ()
  ; List.iter (Buffer.add_char buf) interleaved
  ; Buffer.contents buf
;;

let%expect_test _ =
  ()
  ; print_string @@ merge "ABC" "abc"
  ; [%expect {| AaBbCc |}]
  ; ()
  ; print_string @@ merge "AB" "abcde"
  ; [%expect {| AaBbcde |}]
  ; ()
  ; print_string @@ merge "ABCDE" "ab"
  ; [%expect {| AaBbCDE |}]
  ; ()
  ; print_string @@ merge "abc" "pqr"
  ; [%expect {|apbqcr|}]
  ; ()
  ; print_string @@ merge "ab" "pqrs"
  ; [%expect {|apbqrs|}]
  ; ()
  ; print_string @@ merge "abc" ""
  ; [%expect {|abc|}]
  ; ()
  ; print_string @@ merge "" "ABC"
  ; [%expect {|ABC|}]
  ; ()
  ; print_string @@ merge "" ""
  ; [%expect {||}]
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

let time f =
  let start = Sys.time () in
  let _ = f () in
  let stop = Sys.time () in
  Printf.printf "execution time: %fs\n" (stop -. start)
;;

let inputs n =
  let a = String.init n (fun _ -> 'A') in
  let b = String.init n (fun _ -> 'a') in
  a, b
;;

let (a, b) = inputs 9_999_999
let test1 () = time @@ fun () -> merge a b
let test2 () = time @@ fun () -> merge_imperative a b

(*
   utop # Solution.test1 ();;
   execution time: 6.478412s

   utop # Solution.test2 ();;
   execution time: 1.505136s
*)
