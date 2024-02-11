(*
   https://ocaml.org/docs/functors

   dune exec ./bin/private_type_alias_functor.exe -w

   A more traditional solution can be found here:
   - https://discuss.ocaml.org/t/ann-bwd-2-3-0-for-backward-lists/13278/17
   - https://github.com/biocaml/phylogenetics/blob/master/lib/list1.mli
*)

module Non_empty (Type : sig
    type t
  end) : sig
  type t = private Type.t list

  val init : Type.t * Type.t list -> t
end = struct
  type t = Type.t list

  let init (x, xs) = x :: xs
end

module NE_int_list = Non_empty (struct
    type t = int
  end)

module NE_char_list = Non_empty (struct
    type t = char
  end)

let to_char : NE_int_list.t -> NE_char_list.t =
  fun lst ->
  (* Implementing map generically looks a little bit tricky now *)
  List.map (fun n -> Char.chr (64 + n)) (lst :> int list)
  |> function
  | [] -> assert false
  | x :: xs -> NE_char_list.init (x, xs)
;;

let to_string : NE_char_list.t -> string =
  fun lst ->
  let buf = Buffer.create 16 in
  ()
  ; List.iter (Buffer.add_char buf) (lst :> char list)
  ; Buffer.contents buf
;;

let () =
  let lst = NE_int_list.init (1, [ 2; 3; 4 ]) in
  let first_item = List.hd (lst :> int list) in
  let lst : NE_char_list.t = to_char lst in
  ()
  ; print_endline "Using a paramaterized private type alias..."
  ; Printf.printf "First item: %d\n%!" first_item
  ; Printf.printf "lst=[%s]\n%!" @@ to_string lst (* prints lst=[ABCD] *)
;;
