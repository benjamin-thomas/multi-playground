(* An applicative functor allows applying wrapped functions to wrapped values *)

type 'a my_box = My_box of 'a [@@deriving sexp, compare]

module My_box = Preface.Make.Applicative.Via_pure_and_apply (struct
    type 'a t = 'a my_box

    let pure x = My_box x
    let apply (My_box f) (My_box v) = My_box (f v)
  end)

(*
   Functor
   =======
   Replace with left
   (<$)  :: a        -> f b -> f a
   a (<$) f b :: f a

   Replace with right
   ($>)  :: f a      -> b   -> f b
   f a ($>) b :: f b

   map
   (<$>) :: (a -> b) -> f a -> f b
   (a -> b) <$> f a :: f b

   Applicative functor
   ===================
   Apply:
   val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

   Flipped apply:
   val ( <**> ) : 'a t -> ('a -> 'b) t -> 'b t

   Discard left:
   *>
   unit t -> 'a t -> 'a t

   Discard right:
   <*
   'a t -> unit t -> 'a t
*)

let%test_module "My_box is also a functor" =
  (* Tests copy/pasted from functors.ml *)
  (module struct
    let eq = [%test_eq: Base.int my_box]
    let eq' = [%test_eq: Base.string my_box]
    let tup2 = [%test_eq: (Base.int * Base.int) my_box]
    let tup2' = [%test_eq: (Base.string * Base.string) my_box]
    let double = ( * ) 2
    let repeat x = x ^ x

    (* ints *)
    let%test_unit "functor" =
      ()
      ; eq (My_box 4) (My_box.map double (My_box 2))
      ; eq (My_box 0) (My_box.replace 0 (My_box 2))
    ;;

    let%test_unit "applicative functor" =
      let open My_box in
      ()
      ; eq (pure 4) (apply (pure double) (pure 2))
      ; eq (pure 6) (lift double (pure 3))
      ; eq (pure 12) (lift2 ( * ) (pure 3) (pure 4))
      ; eq (pure 24) (lift3 (fun a b c -> a * b * c) (pure 4) (pure 3) (pure 2))
      ; tup2 (pure (3, 4)) (product (pure 3) (pure 4))
    ;;

    let%test_unit "functor" =
      let open My_box in
      ()
      ; eq (My_box 6) (double <$> My_box 3)
      ; eq (My_box 8) (My_box 4 <&> double)
      ; eq (My_box 0) (My_box 3 $> 0)
      ; eq (My_box 0) (0 <$ My_box 3)
    ;;

    let%test_unit "applicative functor" =
      let open My_box in
      ()
      ; eq (pure 6) (pure double <*> pure 3)
      ; eq (pure 6) (pure 3 <**> pure double)
      ; eq (pure 0) (pure () *> pure 0)
      ; eq (pure 1) (pure 1 <* pure ())
    ;;

    (* strings *)
    let%test_unit "functor" =
      ()
      ; eq' (My_box "aa") (My_box.map repeat (My_box "a"))
      ; eq' (My_box "overwritten!") (My_box.replace "overwritten!" (My_box "a"))
    ;;

    let%test_unit "applicative functor" =
      let open My_box in
      ()
      ; eq' (pure "aa") (apply (pure repeat) (pure "a"))
      ; eq' (pure "aa") (lift repeat (pure "a"))
      ; eq' (pure "ab") (lift2 ( ^ ) (pure "a") (pure "b"))
      ; eq' (pure "ABC") (lift3 (fun a b c -> a ^ b ^ c) (pure "A") (pure "B") (pure "C"))
      ; tup2' (pure ("a", "b")) (product (pure "a") (pure "b"))
    ;;

    let%test_unit "functor" =
      let open My_box in
      ()
      ; eq' (My_box "bbbb") (repeat <$> My_box "bb")
      ; eq' (My_box "overwritten") (My_box "cc" $> "overwritten")
      ; eq' (My_box "stopped!") ("stopped!" <$ My_box "dd")
    ;;

    let%test_unit "applicative functor" =
      let open My_box in
      ()
      ; eq' (pure "aa") (pure repeat <*> pure "a")
      ; eq' (pure "bb") (pure "b" <**> pure repeat)
      ; eq' (pure "c") (pure () *> pure "c")
      ; eq' (pure "d") (pure "d" <* pure ())
    ;;
  end)
;;
