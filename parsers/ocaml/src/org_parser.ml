(* Core provides a handy Date type *)
module Date = Core.Date
module Month = Core.Month

let input =
  {|
SCHEDULED: <2024-09-28 Sun>
DEADLINE: <2024-10-05 Sun>
CLOSED: <2024-10-05 Sun>
|}

let digits n =
  Angstrom.(
    take n >>= fun s ->
    if String.length s <> n then
      fail "invalid len"
    else
      match int_of_string_opt s with
      | None -> fail "oops"
      | Some n -> return n)

let date =
  Angstrom.(
    (
      digits  4  >>= fun y ->
      char   '-' *>
      digits  2  >>| Month.of_int >>= fun m_opt ->
      char   '-' *>
      digits  2  >>= fun d ->
      match m_opt with
      | None ->
        fail "invalid month"
      | Some m ->
          try return (Date.create_exn ~y ~m ~d) with
          _ -> fail "invalid date"
    ) <?> "date"
  )
[@@ocamlformat "disable"]

let%test_unit "parsing dates" =
  let ( => ) = Base.([%test_eq: (Date.t, string) Result.t]) in
  let parse_date = Angstrom.(parse_string ~consume:All date) in
  ()
  ; parse_date "2023-05-27"  => Ok (Date.create_exn ~y:2023 ~m:Month.May ~d:27)
  ; parse_date "2023-053-27" => Error "date: char '-'"
  ; parse_date "2023-13-27"  => Error "date: invalid month"
[@@ocamlformat "disable"]

type org_item = Scheduled of Date.t | Deadline of Date.t | Closed of Date.t
[@@deriving sexp, ord, variants]

let ws =
  Angstrom.(
    skip_while (function
      | ' '
      | '\r'
      | '\n'
      | '\t' ->
          true
      | _ -> false)
    <?> "ws")

let weekday =
  Angstrom.(
    choice
      [ string "Sun"
      ; string "Mon"
      ; string "Tue"
      ; string "Wed"
      ; string "Thu"
      ; string "Fri"
      ; string "Sat"
      ]
    <?> "weekday")

let%test_unit "weekday" =
  let parse_weekday = Angstrom.(parse_string ~consume:All weekday) in
  let eq x = Base.([%test_eq: (string, string) Result.t]) (parse_weekday x) in
  ()
  ; eq "Sun" (Ok "Sun")
  ; eq "Thu" (Ok "Thu")
  ; eq "WAT" (Error "weekday: no more choices")

let org_item =
  Angstrom.(
    let bracket f =
      char '<' *> date >>| f <* ws <* weekday <* char '>'
    in
    let parse_with_date tag f =
      string tag *> ws *> bracket f
    in
    choice
      [ parse_with_date "SCHEDULED:" scheduled
      ; parse_with_date "DEADLINE:"  deadline
      ; parse_with_date "CLOSED:"    closed
      ] <?> "org_item"
  )
[@@ocamlformat "disable"]

let%test_unit "parsing org items" =
  let parse_org_item = Angstrom.(parse_string ~consume:All org_item) in
  let eq x =
    Base.([%test_eq: (org_item, string) Result.t]) (parse_org_item x)
  in
  ()
  ; eq "SCHEDULED: <2024-09-28 Sun>"
      (Ok (Scheduled (Date.create_exn ~y:2024 ~m:Month.Sep ~d:28)))
  ; eq "DEADLINE: <2024-10-05 Sun>"
      (Ok (Deadline (Date.create_exn ~y:2024 ~m:Month.Oct ~d:5)))
  ; eq "CLOSED: <2024-10-05 Sun>"
      (Ok (Closed (Date.create_exn ~y:2024 ~m:Month.Oct ~d:5)))
(* ; eq "CLOSED: <2024-02-30 Sun>" (Error "date: invalid date") *)

let org_items = Angstrom.(ws *> sep_by (char '\n') org_item <* ws)
