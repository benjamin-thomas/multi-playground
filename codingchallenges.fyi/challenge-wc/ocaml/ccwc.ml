[@@@warning "-32-27"]

(* dune exec ./ccwc.exe -w -- -cc ../test.txt *)

let usage =
  {|
  Usage: ccwc [OPTION] FILE
  ccwc is a small command line tool that will count the words in a file.
  |}
  |> Dedent.string
;;

module Switch = struct
  type t =
    | Count_bytes
    | Count_lines
    | Count_words
    | Count_multi_bytes_chars

  let of_string = function
    | "-c" -> Some Count_bytes
    | "-l" -> Some Count_lines
    | "-w" -> Some Count_words
    | "-m" -> Some Count_multi_bytes_chars
    | _ -> None
  ;;
end

let run filepath = function
  | Switch.Count_bytes ->
    let bytes_count = Lib.count_bytes filepath in
    Printf.printf "%d %s\n" bytes_count filepath
  | Switch.Count_lines ->
    let lines_count = Lib.count_lines filepath in
    Printf.printf "%d %s\n" lines_count filepath
  | Switch.Count_words ->
    let words_count = Lib.count_words filepath in
    Printf.printf "%d %s\n" words_count filepath
  | Switch.Count_multi_bytes_chars ->
    let chars_count = Lib.count_multi_bytes_chars filepath in
    Printf.printf "%d %s\n" chars_count filepath
;;

let () = print_newline ()

let () =
  let (option, filepath) =
    try
      let option = Sys.argv.(1) |> Switch.of_string |> Option.get in
      let filepath = Sys.argv.(2) in
      (option, filepath)
    with
    | _ ->
      ()
      ; prerr_endline usage
      ; exit 1
  in
  run filepath option
;;
