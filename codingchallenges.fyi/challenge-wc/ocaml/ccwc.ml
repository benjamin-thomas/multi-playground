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

let count_bytes filepath =
  let aux ic =
    let update_counter acc line = acc + Lib.Bytes.count_line_chars line in
    In_channel.fold_lines update_counter 0 ic
  in
  match filepath with
  | None -> Printf.printf "%d\n" @@ aux In_channel.stdin
  | Some filepath ->
    In_channel.with_open_bin filepath
    @@ fun ic -> Printf.printf "%d %s\n" (aux ic) filepath
;;

let count_lines filepath =
  let aux ic =
    let update_counter acc _ = acc + 1 in
    In_channel.fold_lines update_counter 0 ic
  in
  match filepath with
  | None -> Printf.printf "%d\n" @@ aux In_channel.stdin
  | Some filepath ->
    In_channel.with_open_bin filepath
    @@ fun ic -> Printf.printf "%d %s\n" (aux ic) filepath
;;

let count_words filepath =
  let aux ic =
    let update_counter acc line = acc + Lib.Words.count_line_words line in
    In_channel.fold_lines update_counter 0 ic
  in
  match filepath with
  | None -> Printf.printf "%d\n" @@ aux In_channel.stdin
  | Some filepath ->
    In_channel.with_open_bin filepath
    @@ fun ic -> Printf.printf "%d %s\n" (aux ic) filepath
;;

let count_runes filepath =
  let aux ic =
    let update_counter acc line = acc + Lib.Runes.count_line_runes line in
    In_channel.fold_lines update_counter 0 ic
  in
  match filepath with
  | None -> Printf.printf "%d\n" @@ aux In_channel.stdin
  | Some filepath ->
    In_channel.with_open_bin filepath
    @@ fun ic -> Printf.printf "%d %s\n" (aux ic) filepath
;;

let debug filepath =
  let aux ic =
    let update_counters (lines_count, words_count, bytes_count, runes_count) line =
      ( lines_count + 1
      , words_count + Lib.Words.count_line_words line
      , bytes_count + Lib.Bytes.count_line_chars line
      , runes_count + Lib.Runes.count_line_runes line )
    in
    In_channel.fold_lines update_counters (0, 0, 0, 0) ic
  in
  let (want, got) =
    match filepath with
    | None ->
      let (lines_count, words_count, bytes_count, runes_count) = aux In_channel.stdin in
      ( "lines=7145\twords=58164\tbytes=342190\trunes=339292"
      , Printf.sprintf
          "lines=%d\twords=%d\tbytes=%d\trunes=%d"
          lines_count
          words_count
          bytes_count
          runes_count )
    | Some filepath ->
      In_channel.with_open_bin filepath
      @@ fun ic ->
      let (lines_count, words_count, bytes_count, runes_count) = aux ic in
      let want =
        "lines=7145\twords=58164\tbytes=342190\trunes=339292\tfile=../test.txt"
      in
      let got =
        Printf.sprintf
          "lines=%d\twords=%d\tbytes=%d\trunes=%d\tfile=%s"
          lines_count
          words_count
          bytes_count
          runes_count
          filepath
      in
      (want, got)
  in
  ()
  ; Printf.printf "PASS: %b\n" (want = got)
  ; Printf.printf "WANT: %s\n" want
  ; Printf.printf " GOT: %s\n" got
;;

let count_defaults filepath =
  let aux ic =
    let update_counters (lines_count, words_count, bytes_count) line =
      ( lines_count + 1
      , words_count + Lib.Words.count_line_words line
      , bytes_count + Lib.Bytes.count_line_chars line )
    in
    In_channel.fold_lines update_counters (0, 0, 0) ic
  in
  match filepath with
  | None ->
    let (lines_count, words_count, bytes_count) = aux In_channel.stdin in
    Printf.printf "%d %d %d\n" lines_count words_count bytes_count
  | Some filepath ->
    In_channel.with_open_bin filepath
    @@ fun ic ->
    let (lines_count, words_count, bytes_count) = aux ic in
    Printf.printf "%d %d %d %s\n" lines_count words_count bytes_count filepath
;;

let run is_piped_into =
  let when_normal () =
    match Sys.argv with
    | [| _; "-c"; filepath |] -> count_bytes @@ Some filepath
    | [| _; "-l"; filepath |] -> count_lines @@ Some filepath
    | [| _; "-w"; filepath |] -> count_words @@ Some filepath
    | [| _; "-m"; filepath |] -> count_runes @@ Some filepath
    | [| _; "-debug"; filepath |] -> debug @@ Some filepath
    | [| _; filepath |] -> count_defaults @@ Some filepath
    | _ -> prerr_endline usage
  in
  let when_piped_into () =
    match Sys.argv with
    | [| _; "-c" |] -> count_bytes None
    | [| _; "-l" |] -> count_lines None
    | [| _; "-w" |] -> count_words None
    | [| _; "-m" |] -> count_runes None
    | [| _; "-debug" |] -> debug None
    | [| _ |] -> count_defaults None
    | _ -> failwith "todo other"
  in
  if not is_piped_into then
    when_normal ()
  else
    when_piped_into ()
;;

let () = run (not @@ Unix.isatty Unix.stdin)
