type 'a my_box = My_box of 'a [@@deriving sexp, compare]

(* A functor (not an OCaml functor) represents things which can be mapped over *)
module My_box = Preface.Make.Functor.Via_map (struct
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

   ---

   Map:
   val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

   Flipped map:
   val ( <&> ) : 'a t -> ('a -> 'b) -> 'b t

   Replace left:
   val ( <$ ) : 'a -> 'b t -> 'a t

   Replace right:
   val ( $> ) : 'a t -> 'b -> 'b t
*)

let%test_module _ =
  (module struct
    let eq = [%test_eq: Base.int my_box]
    let eq' = [%test_eq: Base.string my_box]
    let eq'' = [%test_eq: Base.unit my_box]
    let double = ( * ) 2
    let repeat x = x ^ x

    (* ints *)
    let%test_unit _ =
      ()
      ; eq (My_box 4) (My_box.map double (My_box 2))
      ; eq (My_box 0) (My_box.replace 0 (My_box 2))
    ;;

    let%test_unit _ =
      let open My_box in
      ()
      ; eq (My_box 6) (double <$> My_box 3)
      ; eq (My_box 8) (My_box 4 <&> double)
      ; eq (My_box 0) (My_box 3 $> 0)
      ; eq (My_box 0) (0 <$ My_box 3)
    ;;

    (* strings *)
    let%test_unit _ =
      ()
      ; eq' (My_box "aa") (My_box.map repeat (My_box "a"))
      ; eq' (My_box "overwritten") (My_box.replace "overwritten" (My_box "a"))
    ;;

    let%test_unit _ =
      let open My_box in
      ()
      ; eq' (My_box "bbbb") (repeat <$> My_box "bb")
      ; eq' (My_box "cccc") (My_box "cc" <&> repeat)
      ; eq' (My_box "overwritten") (My_box "dd" $> "overwritten")
      ; eq' (My_box "stopped!") ("stopped!" <$ My_box "ee")
    ;;

    (* unit *)
    let%test_unit _ = eq'' (My_box ()) (My_box.void (My_box 2))
    let%test_unit _ = eq'' (My_box ()) (My_box.void (My_box "hello"))
  end)
;;
