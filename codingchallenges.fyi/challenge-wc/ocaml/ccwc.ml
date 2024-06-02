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

let count_bytes filepath ic =
  let bytes_count = Lib.Bytes.count ic in
  Printf.printf "%d %s\n" bytes_count filepath
;;

let count_lines filepath ic =
  let lines_count = Lib.Lines.count ic in
  Printf.printf "%d %s\n" lines_count filepath
;;

let count_words filepath ic =
  let words_count = Lib.Words.count ic in
  Printf.printf "%d %s\n" words_count filepath
;;

let count_runes filepath ic =
  let chars_count = Lib.Runes.count ic in
  Printf.printf "%d %s\n" chars_count filepath
;;

let count_defaults filepath =
  let (lines_count, words_count, bytes_count) =
    ( In_channel.with_open_bin filepath Lib.Lines.count
    , In_channel.with_open_bin filepath Lib.Words.count
    , In_channel.with_open_bin filepath Lib.Bytes.count )
  in
  Printf.printf "%d %d %d %s\n" lines_count words_count bytes_count filepath
;;

let run () =
  match Sys.argv with
  | [| _; "-c"; filepath |] -> In_channel.with_open_bin filepath (count_bytes filepath)
  | [| _; "-l"; filepath |] -> In_channel.with_open_bin filepath (count_lines filepath)
  | [| _; "-w"; filepath |] -> In_channel.with_open_bin filepath (count_words filepath)
  | [| _; "-m"; filepath |] -> In_channel.with_open_bin filepath (count_runes filepath)
  | [| _; filepath |] -> count_defaults filepath
  | _ -> prerr_endline usage
;;

let is_piped_into = not @@ Unix.isatty Unix.stdin

let () =
  Printf.printf
    "%d => %b [%s]\n"
    (Unix.getpid ())
    is_piped_into
    (Array.to_list Sys.argv |> List.tl |> String.concat "|")
;;

let () = run ()
