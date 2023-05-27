(*
   dune runtest -w
 *)

type date = { year : int; month : int; day : int }

let ( let* ) = Option.bind

let parse_int len str =
  if String.length str = len then
    int_of_string_opt str
  else
    None
;;

let date_of_string str =
  match String.split_on_char '.' str with
  | [ year; month; day ] ->
      let* year = int_of_string_opt year in
      let* month = parse_int 2 month in
      let* day = parse_int 2 day in
      Some { year; month; day }
  | _ -> None
;;

let%test "empty" = None = date_of_string ""

let%test "valid" =
  Some { year = 2023; month = 5; day = 26 } = date_of_string "2023.05.26"
;;

let%test "valid" =
  Some { year = 10000; month = 5; day = 26 } = date_of_string "10000.05.26"
;;

let%test "bogus day" = None = date_of_string "2023.05.123"
let%test "bogus month" = None = date_of_string "2023.0z.12"
