(*
   https://oj.moonbitlang.com/problems/202412-004-sum-of-even-digits

   dune exec ./test/test.exe test 004 -w
*)

let to_chars str = String.fold_right (fun c acc -> c :: acc) str []

let combinations xss =
  List.fold_right
    begin
      fun xs acc ->
        List.concat_map
          begin
            fun x -> List.map (fun ys -> x :: ys) acc
          end
          xs
    end
    xss
    [ [] ]
;;

let solution lst =
  let lst2 = List.map to_chars lst in
  combinations lst2
  |> List.filter
       begin
         fun (nums : char list) ->
           let sum =
             List.fold_left (fun tot n -> n + tot) 0 (List.map int_of_char nums)
           in
           sum mod 2 = 0
       end
  |> List.length
;;
