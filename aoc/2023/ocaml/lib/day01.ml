let to_int c = int_of_char c - int_of_char '0'
let chars_of_string s = List.init (String.length s) (String.get s)

let rec filter_nums1 = function
  | [] -> []
  | ('0' .. '9' as h) :: t -> h :: filter_nums1 t
  | _ :: t -> filter_nums1 t
;;

let rec filter_nums2 = function
  | [] -> []
  | ('0' .. '9' as h)  :: t                     ->  h  :: filter_nums2 t
  | 'o' :: ('n' :: 'e' :: _ as t)               -> '1' :: filter_nums2 t
  | 't' :: ('w' :: 'o' :: _ as t)               -> '2' :: filter_nums2 t
  | 't' :: ('h' :: 'r' :: 'e' :: 'e' :: _ as t) -> '3' :: filter_nums2 t
  | 'f' :: ('o' :: 'u' :: 'r' :: _ as t)        -> '4' :: filter_nums2 t
  | 'f' :: ('i' :: 'v' :: 'e' :: _ as t)        -> '5' :: filter_nums2 t
  | 's' :: ('i' :: 'x' :: _ as t)               -> '6' :: filter_nums2 t
  | 's' :: ('e' :: 'v' :: 'e' :: 'n' :: _ as t) -> '7' :: filter_nums2 t
  | 'e' :: ('i' :: 'g' :: 'h' :: 't' :: _ as t) -> '8' :: filter_nums2 t
  | 'n' :: ('i' :: 'n' :: 'e' :: _ as t)        -> '9' :: filter_nums2 t
  | _ :: t -> filter_nums2 t
[@@ocamlformat "disable"]

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

let part1 = process_lines filter_nums1
let part2 = process_lines filter_nums2
