(*
 * dune exec ./bin/main.exe ./to_export.ml
 *)

module To_elm = Internal.Ocaml_to_elm

let filename =
  try Sys.argv.(1) with
  | _ -> failwith "Must give filename"
;;

let () =
  let ic = open_in filename in
  let input = In_channel.input_all ic in
  let to_record = To_elm.(make_parser parse_record) in
  match to_record input with
  | Error err -> failwith @@ "Could not convert this file to Elm: " ^ err
  | Ok record ->
      let output = To_elm.to_elm record in
      print_endline output
;;
