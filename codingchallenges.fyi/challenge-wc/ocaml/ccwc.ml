[@@@warning "-32-27"]

(* dune exec ./ccwc.exe -w -- -cc ../test.txt *)

let usage =
  {|
  Usage: ccwc [OPTION] FILE
  ccwc is a small command line tool that will count the words in a file.
  |}
  |> Dedent.string
;;

let () = print_newline ()

let () =
  match Sys.argv with
  | [| _; "-c"; filepath |] ->
    let bytes_count = Lib.count_bytes filepath in
    Printf.printf "%d %s\n" bytes_count filepath
  | [| _; "-l"; filepath |] ->
    let lines_count = Lib.count_lines filepath in
    Printf.printf "%d %s\n" lines_count filepath
  | [| _; "-w"; filepath |] ->
    let words_count = Lib.count_words filepath in
    Printf.printf "%d %s\n" words_count filepath
  | [| _; "-m"; filepath |] ->
    let chars_count = Lib.count_multi_bytes_chars filepath in
    Printf.printf "%d %s\n" chars_count filepath
  | [| _; filepath |] ->
    let bytes_count = Lib.count_bytes filepath in
    let lines_count = Lib.count_lines filepath in
    let words_count = Lib.count_words filepath in
    let chars_count = Lib.count_multi_bytes_chars filepath in
    Printf.printf
      "%d %d %d %d %s\n"
      bytes_count
      lines_count
      words_count
      chars_count
      filepath
  | _ -> prerr_endline usage
;;
