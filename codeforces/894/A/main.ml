(*

spell: disable-next-line
time ocaml ./main.ml < <(echo "QQAAQASGAYAAAAKAKAQIQEAQAIAAIAQQQQQ")

*)

let example = "QAQAQYSYIOIWIN" (* spell: disable-line *)

let example2 = [1;2;3;4;5]

let to_char_list str =
  let acc = ref [] in
  for i = String.length str - 1 downto 0 do
    acc := str.[i] :: !acc
  done;
  !acc

(** [combs3] returns the combinations of [lst], as in N choose 3.
*)
let combs3 lst =
  let (let*) xs f = List.concat_map f xs in
  let pure x = [x] in
  let empty = [] in (* stops the computation in the monadic context *)
  let guard p = if p then pure () else empty in
  let* (i, x) = List.mapi (fun i n -> (i, n)) lst in
  let* (j, y) = List.mapi (fun i n -> (i, n)) lst in
  let* (k, z) = List.mapi (fun i n -> (i, n)) lst in
  let* () = guard (i < j && j < k) in
  pure [x; y; z]

let combs3b lst =
  let lst2 = (List.mapi (fun i n -> (i, n)) lst) in
  List.concat_map
    (fun (i, x) ->
       List.concat_map
         (fun (j, y) ->
            List.concat_map
              (fun (k, z) ->
                 if i < j && j < k then [[x; y; z]] else []
              )
              lst2
         )
         lst2
    )
    lst2

let solution str =
  str
  |> to_char_list
  |> combs3
  |> List.filter (fun xs -> xs = ['Q'; 'A'; 'Q'])
  |> List.length

(*

To optimize for optimal performance, run:

ocamlopt -o main main.ml

*)

let () =
  let line = read_line () in
  print_int @@ solution line