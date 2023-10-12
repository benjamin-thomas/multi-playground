module Debug_runtime = Minidebug_runtime.PrintBox (struct
  let debug_ch = stdout
  let time_tagged = false
end)
(*
let%debug_show rec sub_sets (input : char list) : char list list =
  match input with
  | [] -> [ [] ]
  | h :: t ->
      let left : char list list = sub_sets t in
      let right : char list list = List.map (List.cons h) left in
      left @ right
;; *)

let%debug_show sets_of n (lst : char list) =
  let rec sub_sets (lst : char list) (n : int) : char list list =
    match lst with
    | [] -> [ [] ]
    | h :: t ->
        let left = sub_sets t (n - 1) in
        let right =
          if n > 0 then
            List.map (List.cons h) left
          else
            []
        in
        left @ right
  in
  sub_sets lst n
;;

let () =
  let _ = sets_of 2 [ 'A'; 'B'; 'C' ] in
  print_endline "Done!"
;;
