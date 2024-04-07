[@@@warning "-32"]

let table (str : string) : char list list =
  str
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map (fun line -> List.init (String.length line) (String.get line))
;;

let to_matrix lst = Array.of_list (List.map Array.of_list lst)

let neighbours ~x ~y ~width ~height =
  let within_bounds (x, y) = x >= 0 && x < width && y >= 0 && y < height in
  let candidates =
    [ (x - 1, y - 1) ; (x, y - 1) ; (x + 1, y - 1)
    ; (x - 1, y)     ; (x + 1, y)
    ; (x - 1, y + 1) ; (x, y + 1) ; (x + 1, y + 1)
    ]
  in
  candidates |> List.filter within_bounds
[@@ocamlformat "disable"]

let neighbouring_parts matrix ~x ~y ~width ~height =
  let neighbours = neighbours ~x ~y ~width ~height in
  neighbours |> List.map (fun (x, y) -> matrix.(y).(x)) |> List.filter (fun x -> x <> '.')
;;

let sum_of_parts lst =
  let matrix = to_matrix lst in
  let width = 10 in
  let height = 10 in
  (* generate a list of all xy coordinates *)
  let coords =
    List.concat @@ List.init width (fun x -> List.init height (fun y -> (x, y)))
  in
  let parts =
    List.concat_map (fun (x, y) -> neighbouring_parts matrix ~x ~y ~width ~height) coords
  in
  parts
  |> List.fold_left
       (fun acc -> function
         | '0' .. '9' as n ->
           let n' = int_of_char n - int_of_char '0' in
           let () = Printf.eprintf "%1d + %3d = %3d\n" n' acc (acc + n') in
           acc + n'
         | _ -> acc)
       0
;;
