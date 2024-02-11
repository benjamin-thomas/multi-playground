(*
   https://discuss.ocaml.org/t/tutorial-on-gadts/14040/15

   dune exec ./bin/private_type_alias.exe -w
*)

module Int_NE : sig
  type t = private int list

  val init : int * int list -> t
end = struct
  type t = int list

  let init (x, xs) = x :: xs
end

let () =
  let lst : Int_NE.t = Int_NE.init (1, [ 2; 3; 4 ]) in
  let to_print =
    (lst :> int list)
    |> List.map (fun n -> Char.chr (64 + n))
    |> fun lst ->
    let buf = Buffer.create 16 in
    ()
    ; List.iter (Buffer.add_char buf) lst
    ; Buffer.contents buf
  in
  ()
  ; print_endline "Using a private type alias..."
  ; Printf.printf "lst=[%s]\n%!" @@ to_print (* prints lst=[ABCD] *)
;;
