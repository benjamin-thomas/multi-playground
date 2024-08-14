type query = { select : string; from : string; where : string; limit : int }

(* Steps *)
type empty
type with_select
type with_from
type with_where

(* Outcome *)
type 'a partial_query = Partial of query
type full_query = Full of query

let empty_query : empty partial_query =
  Partial { select = ""; from = ""; where = ""; limit = 0 }

let set_select : string -> empty partial_query -> with_select partial_query =
 fun select (Partial query) -> Partial { query with select }

let set_from : string -> with_select partial_query -> with_from partial_query =
 fun from (Partial query) -> Partial { query with from }

let set_where : string -> with_from partial_query -> with_where partial_query =
 fun where (Partial query) -> Partial { query with where }

let set_limit : int -> with_where partial_query -> full_query =
 fun limit (Partial query) -> Full { query with limit }

let string_of_query (Full query) =
  let { select; from; where; limit } = query in
  Printf.sprintf "SELECT %s FROM %s WHERE %s LIMIT %d" select from where limit

[@@@ocamlformat "disable"]
let%test_unit _ =
let ( => ) = [%test_eq: Base.string] in
   let qry  : full_query =
    empty_query
    |> set_select "*"
    |> set_from "bobby_table"
    |> set_where "some_value >= 3"
    |> set_limit 10
   in
   string_of_query qry
     => "SELECT * FROM bobby_table WHERE some_value >= 3 LIMIT 10"
