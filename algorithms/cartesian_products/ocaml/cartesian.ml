(*
   dune runtest --watch

   Inspired from explorations at:

   https://github.com/benjamin-thomas/gpw-haskell/blob/caede8a17b80e29b10c41606f34589383d2bc488/lesson17/probabilities.hs#L96
*)

module WithCore = struct
  open Core

  let cross_product f xs ys =
    List.concat_map ~f:(fun x -> List.map ~f:(fun y -> f x y) ys) xs
  ;;

  let cross_product2 f xs ys =
    let ( let* ) lst f = List.bind lst ~f in
    let* x = xs in
    let* y = ys in
    List.return (f x y)
  ;;

  let cross_product3 f xs ys =
    let ( let* ) lst f = List.bind lst ~f in
    let* x = xs in
    List.map ~f:(fun y -> f x y) ys
  ;;

  let cross_product4 f xs ys =
  List.Let_syntax.(
    xs >>= fun x ->
    ys >>| fun y ->
    f x y
  )
[@@ocamlformat "disable"]

  let cross_product5 f xs ys =
  let open List.Cartesian_product in
  let (<$>) f lst = List.map lst ~f in
  f <$> xs <*> ys
[@@ocamlformat "disable"]

  let cross_product6 f xs ys = List.Cartesian_product.(map ~f xs <*> ys)

  let%expect_test _ =
    let test f xs ys = print_s [%sexp (f (fun x y -> x, y) xs ys : (int * int) list)] in
    test cross_product [ 1; 2; 3 ] [ 4; 5; 6 ];
    [%expect {| ((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)) |}];
    test cross_product2 [ 1; 2; 3 ] [ 4; 5; 6 ];
    [%expect {| ((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)) |}];
    test cross_product3 [ 1; 2; 3 ] [ 4; 5; 6 ];
    [%expect {| ((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)) |}];
    test cross_product4 [ 1; 2; 3 ] [ 4; 5; 6 ];
    [%expect {| ((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)) |}];
    test cross_product5 [ 1; 2; 3 ] [ 4; 5; 6 ];
    [%expect {| ((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)) |}];
    test cross_product6 [ 1; 2; 3 ] [ 4; 5; 6 ];
    [%expect {| ((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)) |}];
    ()
  ;;
end

module WithStandardLib = struct
  let cross_product f xs ys =
    List.concat_map (fun x -> List.concat_map (fun y -> [ f x y ]) ys) xs
  ;;

  let cross_product2 f xs ys = List.concat_map (fun x -> List.map (fun y -> f x y) ys) xs

  let cross_product3 f xs ys =
    let ( let* ) = Fun.flip List.concat_map in
    let* x = xs in
    let* y = ys in
    [ f x y ]
  ;;

  let cross_product4 f xs ys =
    let ( let* ) = Fun.flip List.concat_map in
    let* x = xs in
    List.map (fun y -> f x y) ys
  ;;

  (* <*> AKA `apply` *)
  let ( <*> ) fs xs = List.concat_map (fun f -> List.map f xs) fs
  let ( <$> ) f xs = List.map f xs

  (*  *)
  let cross_product5 f xs ys = List.map f xs <*> ys
  let cross_product6 f xs ys = f <$> xs <*> ys

  let%expect_test _ =
    let test f xs ys =
      Core.(print_s [%sexp (f (fun x y -> x, y) xs ys : (int * int) list)])
    in
    test cross_product [ 1; 2; 3 ] [ 4; 5; 6 ];
    [%expect {| ((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)) |}];
    test cross_product2 [ 1; 2; 3 ] [ 4; 5; 6 ];
    [%expect {| ((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)) |}];
    test cross_product3 [ 1; 2; 3 ] [ 4; 5; 6 ];
    [%expect {| ((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)) |}];
    test cross_product4 [ 1; 2; 3 ] [ 4; 5; 6 ];
    [%expect {| ((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)) |}];
    test cross_product5 [ 1; 2; 3 ] [ 4; 5; 6 ];
    [%expect {| ((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)) |}];
    test cross_product6 [ 1; 2; 3 ] [ 4; 5; 6 ];
    [%expect {| ((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)) |}];
    ()
  ;;
end
