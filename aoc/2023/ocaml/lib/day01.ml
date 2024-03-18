let to_int c = int_of_char c - int_of_char '0'
let chars_of_string s = List.init (String.length s) (String.get s)

module Part1 = struct
  let rec filter_nums = function
    | [] -> []
    | ('0' .. '9' as h) :: t -> h :: filter_nums t
    | _ :: t -> filter_nums t
  ;;
end

module Part2 = struct
  let rec filter_nums = function
  | [] -> []
  | ('0' .. '9'  as h)  :: t                     ->  h  :: filter_nums t
  |  'o' :: ('n' :: 'e' :: _ as t)               -> '1' :: filter_nums t
  |  't' :: ('w' :: 'o' :: _ as t)               -> '2' :: filter_nums t
  |  't' :: ('h' :: 'r' :: 'e' :: 'e' :: _ as t) -> '3' :: filter_nums t
  |  'f' :: ('o' :: 'u' :: 'r' :: _ as t)        -> '4' :: filter_nums t
  |  'f' :: ('i' :: 'v' :: 'e' :: _ as t)        -> '5' :: filter_nums t
  |  's' :: ('i' :: 'x' :: _ as t)               -> '6' :: filter_nums t
  |  's' :: ('e' :: 'v' :: 'e' :: 'n' :: _ as t) -> '7' :: filter_nums t
  |  'e' :: ('i' :: 'g' :: 'h' :: 't' :: _ as t) -> '8' :: filter_nums t
  |  'n' :: ('i' :: 'n' :: 'e' :: _ as t)        -> '9' :: filter_nums t
  |   _  :: t -> filter_nums t
  [@@ocamlformat "disable"]
end

let first_last = function
  | [] -> (0, 0)
  | x :: xs -> List.fold_left (fun _ y -> (x, y)) (x, x) xs
;;

let process_line filter_fn line =
  first_last @@ List.map to_int @@ filter_fn @@ chars_of_string line
;;

let process_lines filter_fn lines =
  lines
  |> List.map (process_line filter_fn)
  |> List.fold_left (fun acc (first, last) -> acc + (first * 10) + last) 0
;;

let part1 = process_lines Part1.filter_nums
let part2 = process_lines Part2.filter_nums
