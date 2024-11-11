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

module Occurrences = struct
  module IntMap = Map.Make (Int)

  let make =
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

  let sort map =
    map
    |> IntMap.to_list
    |> List.sort (fun a b ->
      match compare (snd b) (snd a) with
      | 0 -> compare (fst b) (fst a)
      | v -> v)
  ;;

  let sum n lst = lst |> take n |> List.fold_left (fun acc (k, v) -> acc + (k * v)) 0
end

let rec drop n lst =
  match lst with
  | [] -> []
  | x :: xs -> if n > 0 then drop (n - 1) xs else x :: xs
;;

let rec make_windows n lst =
  match take n lst with
  | [] -> []
  | window -> window :: make_windows n (drop 1 lst)
;;

let ( >> ) f g x = g (f x)

let solve lst k x =
  make_windows k lst
  |> List.map Occurrences.(make >> sort >> sum x)
  |> List.fold_left ( + ) 0
;;
