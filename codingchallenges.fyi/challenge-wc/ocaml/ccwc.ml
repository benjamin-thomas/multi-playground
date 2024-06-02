(*
   dune exec ./ccwc.exe -w -- -c ../test.txt
   dune exec ./ccwc.exe -w --display=quiet --no-print-directory -- -debug ../test.txt

   rg --files | entr -c bash -c 'dune build && _build/default/ccwc.exe -c ../test.txt'
   rg --files | entr -c bash -c 'dune build && cat ../test.txt | _build/default/ccwc.exe -c'
*)

let usage =
  {|
  Usage: ccwc [OPTION] FILE
  ccwc is a small command line tool that will count the words in a file.
  |}
  |> Dedent.string
;;

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

let debug filepath =
  let (lines_count, words_count, bytes_count, runes_count) =
    ( In_channel.with_open_bin filepath Lib.Lines.count
    , In_channel.with_open_bin filepath Lib.Words.count
    , In_channel.with_open_bin filepath Lib.Bytes.count
    , In_channel.with_open_bin filepath Lib.Runes.count )
  in
  let want = "lines=7145\twords=58164\tbytes=342190\trunes=339292\tfile=../test.txt" in
  let got =
    Printf.sprintf
      "lines=%d\twords=%d\tbytes=%d\trunes=%d\tfile=%s"
      lines_count
      words_count
      bytes_count
      runes_count
      filepath
  in
  ()
  ; Printf.printf "PASS: %b\n" (want = got)
  ; Printf.printf "WANT: %s\n" want
  ; Printf.printf " GOT: %s\n" got
;;

let count_defaults_stdin () =
  (* Wrong approach
     Maybe I should handle one line at a time (with a Seq.t?)

     So rather than consume a In_channel, I should consume a `line Seq.t`
     First I should try to use `In_channel.input_line ic` every where (don't consume single chars)
  *)
  let (lines_count, words_count) =
    (Lib.Lines.count In_channel.stdin, Lib.Words.count In_channel.stdin)
  in
  (* let () = In_channel.seek stdin 0L in *)
  Printf.printf "%d %d\n" lines_count words_count
;;

let run is_piped_into =
  let when_normal () =
    match Sys.argv with
    | [| _; "-c"; filepath |] -> In_channel.with_open_bin filepath (count_bytes filepath)
    | [| _; "-l"; filepath |] -> In_channel.with_open_bin filepath (count_lines filepath)
    | [| _; "-w"; filepath |] -> In_channel.with_open_bin filepath (count_words filepath)
    | [| _; "-m"; filepath |] -> In_channel.with_open_bin filepath (count_runes filepath)
    | [| _; "-debug"; filepath |] -> debug filepath
    | [| _; filepath |] -> count_defaults filepath
    | _ -> prerr_endline usage
  in
  let when_piped_into () =
    match Sys.argv with
    | [| _; "-c" |] -> count_bytes "" In_channel.stdin
    | [| _; "-l" |] -> count_lines "" In_channel.stdin
    | [| _; "-w" |] -> count_words "" In_channel.stdin
    | [| _ |] -> count_defaults_stdin ()
    | _ -> failwith "todo other"
  in
  if not is_piped_into then
    when_normal ()
  else
    when_piped_into ()
;;

let () = run (not @@ Unix.isatty Unix.stdin)
