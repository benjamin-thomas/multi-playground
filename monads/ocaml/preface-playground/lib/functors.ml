type 'a my_box = My_box of 'a [@@deriving sexp, compare]

module Box = Preface.Make.Functor.Via_map (struct
    type 'a t = 'a my_box

    let map f (My_box x) = My_box (f x)
  end)

(*
   Replace with left
   (<$)  :: a        -> f b -> f a
   a (<$) f b :: f a

   Replace with right
   ($>)  :: f a      -> b   -> f b
   f a ($>) b :: f b

   map
   (<$>) :: (a -> b) -> f a -> f b
   (a -> b) <$> f a :: f b
*)

let%test_module _ =
  (module struct
    let eq = [%test_eq: Base.int my_box]
    let eq' = [%test_eq: Base.string my_box]
    let double = ( * ) 2
    let repeat x = x ^ x

    (* ints *)
    let%test_unit _ = eq (My_box 4) (Box.map double (My_box 2))

    let%test_unit _ =
      let open Box in
      eq (My_box 6) (double <$> My_box 3)
    ;;

    let%test_unit _ =
      let open Box in
      ()
      ; eq (My_box 6) (double <$> My_box 3)
      ; eq (My_box 0) (My_box 3 $> 0)
      ; eq (My_box 0) (0 <$ My_box 3)
    ;;

    (* strings *)
    let%test_unit _ = eq' (My_box "aa") (Box.map repeat (My_box "a"))

    let%test_unit _ =
      let open Box in
      ()
      ; eq' (My_box "bbbb") (repeat <$> My_box "bb")
      ; eq' (My_box "overwritten") (My_box "cc" $> "overwritten")
      ; eq' (My_box "stopped!") ("stopped!" <$ My_box "dd")
    ;;
  end)
;;
