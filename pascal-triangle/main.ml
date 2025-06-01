
(*

[pascal-triangle]$ ocaml ./main.ml
[1]
[1; 1]
[1; 2; 1]
[1; 3; 3; 1]
[1; 4; 6; 4; 1]
[1; 5; 10; 10; 5; 1]
[1; 6; 15; 20; 15; 6; 1]
[1; 7; 21; 35; 35; 21; 7; 1]
[1; 8; 28; 56; 70; 56; 28; 8; 1]
[1; 9; 36; 84; 126; 126; 84; 36; 9; 1]

*)

let pascal =
  let gen_row row =
    let rec pairs = function
      | [] | [ _ ] -> []
      | x :: y :: rest -> (x, y) :: pairs (y :: rest)
    in
    let middle = List.map (fun (x, y) -> x + y) (pairs row) in
    [ 1 ] @ middle @ [ 1 ]
  in
  Seq.unfold (fun xs -> Some (xs, gen_row xs)) [ 1 ]
;;

let print_row row =
  print_string "[";
  (match row with
   | [] -> ()
   | x :: xs ->
     print_int x;
     List.fold_left
       (fun () y ->
          print_string "; ";
          print_int y)
       ()
       xs);
  print_endline "]"
;;

let () = List.iter print_row @@ List.of_seq @@ Seq.take 10 pascal
