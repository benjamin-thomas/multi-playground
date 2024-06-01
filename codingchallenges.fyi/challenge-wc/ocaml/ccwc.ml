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
  type t = Count_bytes

  let of_string = function
    | "-c" -> Some Count_bytes
    | _ -> None
  ;;
end

let run filepath = function
  | Switch.Count_bytes ->
    let bytes = Lib.count_bytes filepath in
    Printf.printf "%d %s\n" bytes filepath
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
