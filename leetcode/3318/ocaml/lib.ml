module IntMap = Map.Make (Int)

let occurrences =
  List.fold_left
    (fun acc n ->
      IntMap.update
        n
        (function
          | None -> Some 1
          | Some prev -> Some (prev + 1))
        acc)
    IntMap.empty
;;

let sorted_occurrences map =
  map
  |> IntMap.to_list
  |> List.sort (fun a b ->
    match compare (snd b) (snd a) with
    | 0 -> compare (fst b) (fst a)
    | v -> v)
;;

(** Takes at least [n] elements in the [lst] list.
    Otherwise, an empty list is returned. *)
let take n lst =
  let rec aux acc n lst =
    match lst with
    | [] -> if n = 0 then acc else []
    | x :: xs -> if n = 0 then acc else aux (x :: acc) (n - 1) xs
  in
  aux [] n lst |> List.rev
;;

let sum_occurences n lst =
  lst |> take n |> List.fold_left (fun acc (k, v) -> acc + (k * v)) 0
;;

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

let rec drop n lst =
  match lst with
  | [] -> []
  | x :: xs -> if n > 0 then drop (n - 1) xs else x :: xs
;;

let rec make_windows n lst =
  match take n lst with
  | [] -> []
  | xs -> xs :: make_windows n (drop 1 lst)
;;

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

let solve lst k x =
  let intermediate =
    let make_occ lst = sum_occurences x @@ sorted_occurrences @@ occurrences lst in
    make_windows k lst |> List.map make_occ
  in
  List.fold_left (fun acc x -> acc + x) 0 intermediate
;;

(*
   OK
   utop # Lib.solve [1;1;2;2;3;4;2;3] 6 2;;
   - : int = 28

   ---

   OK
   utop # Lib.solve [3;8;7;8;7;5] 2 2;;
   - : int = 6
*)
