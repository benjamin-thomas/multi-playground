type query = { select : string; from : string; where : string; limit : int }

(* Phantom types to represent the state of each field *)
type empty
type select_set
type from_set
type where_set
type limit_set

(* Bitmask-like type to track field states *)
type ('s, 'f, 'w, 'l) partial_query = Partial of query
type full_query = Full of query

(* Initial empty query *)
let empty_query : (empty, empty, empty, empty) partial_query =
  Partial { select = ""; from = ""; where = ""; limit = 0 }

(* Setters update the query and change the phantom type to indicate the field is set *)
let set_select :
    string ->
    ('s, 'f, 'w, 'l) partial_query ->
    (select_set, 'f, 'w, 'l) partial_query =
 fun select (Partial query) -> Partial { query with select }

let set_from :
    string ->
    ('s, 'f, 'w, 'l) partial_query ->
    ('s, from_set, 'w, 'l) partial_query =
 fun from (Partial query) -> Partial { query with from }

let set_where :
    string ->
    ('s, 'f, 'w, 'l) partial_query ->
    ('s, 'f, where_set, 'l) partial_query =
 fun where (Partial query) -> Partial { query with where }

let set_limit :
    int ->
    ('s, 'f, 'w, 'l) partial_query ->
    ('s, 'f, 'w, limit_set) partial_query =
 fun limit (Partial query) -> Partial { query with limit }

let build_query :
    (select_set, from_set, where_set, limit_set) partial_query -> full_query =
 fun (Partial query) -> Full query

let string_of_query (Full query) =
  let { select; from; where; limit } = query in
  Printf.sprintf "SELECT %s FROM %s WHERE %s LIMIT %d" select from where limit

[@@@ocamlformat "disable"]
let%test_unit _ =
   let qry = (* any order! *)
     empty_query
     |> set_from "bobby_table"
     |> set_where "some_value >= 3"
     |> set_limit 10
     |> set_select "*"
    |> build_query
   in
   let qry2 = (* any order! *)
    empty_query
    |> set_where "some_value >= 4"
    |> set_from "waaaaaaaatt"
    |> set_limit 99
    |> set_select "x"
   |> build_query
  in


   [%test_eq: Base.string * Base.string]
   ( string_of_query qry
   , string_of_query qry2
   )
   ( "SELECT * FROM bobby_table WHERE some_value >= 3 LIMIT 10"
   , "SELECT x FROM waaaaaaaatt WHERE some_value >= 4 LIMIT 99"
   )
