module IntMap = Map.Make (Int)

let occurrences =
  List.fold_left
    (fun acc n ->
      IntMap.update
        n
        (function
          | None -> Some 1
          | Some prev -> Some (prev + 1))
        acc)
    IntMap.empty
;;

let sorted_occurrences map =
  map
  |> IntMap.to_list
  |> List.sort (fun a b ->
    match compare (snd b) (snd a) with
    | 0 -> compare (fst b) (fst a)
    | v -> v)
;;

(** Takes at least [n] elements in the [lst] list.
    Otherwise, an empty list is returned. *)
let take n lst =
  let rec aux acc n lst =
    match lst with
    | [] -> if n = 0 then acc else []
    | x :: xs -> if n = 0 then acc else aux (x :: acc) (n - 1) xs
  in
  aux [] n lst |> List.rev
;;

let sum_occurences n lst =
  lst |> take n |> List.fold_left (fun acc (k, v) -> acc + (k * v)) 0
;;

let rec drop n lst =
  match lst with
  | [] -> []
  | x :: xs -> if n > 0 then drop (n - 1) xs else x :: xs
;;

let rec make_windows n lst =
  match take n lst with
  | [] -> []
  | xs -> xs :: make_windows n (drop 1 lst)
;;

let solve lst k x =
  make_windows k lst
  |> List.map (fun lst -> lst |> occurrences |> sorted_occurrences |> sum_occurences x)
  |> List.fold_left (fun acc x -> acc + x) 0
;;
