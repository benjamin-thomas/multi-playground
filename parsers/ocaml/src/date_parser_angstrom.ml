module A = Angstrom
module B = Base

let ( >>= ) = A.( >>= )
let ( >>| ) = A.( >>| )

type date = { year : B.int; month : B.int; day : B.int } [@@deriving sexp, ord]

(* chomp one ore more digits *)
let digits : int A.t =
  A.take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>| int_of_string
;;

module Parser = Angstrom

let digits2 : int Parser.t =
  let take_digits =
    Parser.take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  Parser.map ~f:int_of_string take_digits
;;

let digits3 : int Parser.t =
  let take_digits =
    Parser.take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  Parser.bind
    ~f:(fun x ->
      match int_of_string_opt x with
      | Some n -> Parser.return n
      | None -> Parser.fail "impossible")
    take_digits
;;

(** [fromOption] assumes that applying its function parameter will never return [None].
 * If it does, the parsing fails.
 *)
let from_option = function
  | Some n -> Parser.return n
  | None -> Parser.fail "impossible"
;;

let ( >> ) f g x = f (g x)

let digits4 : int Parser.t =
  let take_digits =
    Parser.take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  Parser.bind ~f:(from_option >> int_of_string_opt) take_digits
;;

let digits5 : int Parser.t =
  let take_digits =
    Parser.take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  let f s = from_option (int_of_string_opt s) in
  Parser.bind ~f take_digits
;;

let digits6 : int Parser.t =
  let take_digits =
    Parser.take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in

  let to_int s =
    int_of_string_opt s |> function
    | Some n -> Parser.return n
    | None -> Parser.fail "there must be a bug in take_digits (!??)"
  in
  Parser.bind ~f:to_int take_digits
;;

(* chomp exactly `n` digits *)
let digits' n =
  A.take n >>= fun s ->
  if String.length s <> n then
    A.fail "invalid len"
  else
    match int_of_string_opt s with
    | None -> A.fail "oops"
    | Some n -> A.return n
;;

let%test_unit "parsing digits" =
  let ( => ) = [%test_eq: (B.int, B.string) B.Result.t] in
  let parse_digits = A.parse_string ~consume:All digits in
  let parse_digits' s = parse_digits s |> Result.map_error (fun _ -> "Digit err: " ^ s) in
  ()
  ; parse_digits  "1"   => Ok 1
  ; parse_digits  "2"   => Ok 2
  ; parse_digits  "12"  => Ok 12
  ; parse_digits  "12x" => Error ": end_of_input"
  ; parse_digits' "12x" => Error "Digit err: 12x"
  ; parse_digits' "12"  => Ok 12
[@@ocamlformat "disable"]

(* a date parser, not quite valid *)
let date =
  digits >>= fun year ->
  A.char '.' >>= fun _ ->
  digits     >>= fun month ->
  A.char '.' >>= fun _ ->
  digits     >>= fun day ->
  A.return { year; month ;day }
[@@ocamlformat "disable"]

(* a date parser, better *)
let date' =
  digits     >>= fun year ->
  A.char '.' >>= fun _ ->
  digits' 2  >>= fun month ->
  A.char '.' >>= fun _ ->
  digits' 2  >>= fun day ->
  A.return { year; month ;day }
[@@ocamlformat "disable"]

let%test_unit "parsing dates" =
  let ( => ) = [%test_eq: (date, B.string) B.Result.t] in
  let parse_date = A.parse_string ~consume:All date in
  ()
  ; parse_date "2023.05.27"  => Ok { year = 2023; month = 5; day = 27 }
  ; parse_date "2023.123.27" => Ok { year = 2023; month = 123; day = 27 } (* oops! *)
[@@ocamlformat "disable"]

let%test_unit "parsing dates, better" =
  let ( => ) = [%test_eq: (date, B.string) B.Result.t] in
  let parse_date = A.parse_string ~consume:All date' in
  ()
  ; parse_date "2023.05.27"  => Ok { year = 2023; month = 5; day = 27 }
  ; parse_date "2023.053.27" => Error ": char '.'" (* I'm not sure if trying to make the error message better is worth pursuing *)
[@@ocamlformat "disable"]
