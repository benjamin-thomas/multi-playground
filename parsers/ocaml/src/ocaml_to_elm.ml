module A = Angstrom
module B = Base

let ( >>= ) = A.( >>= )
let ( *> ) = A.( *> )
let ( <* ) = A.( <* )

open Base
open Printf

type record_attr = Record_attr of string * string list [@@deriving sexp, ord]
type record = Record of string * record_attr list [@@deriving sexp, ord]

let is_whitespace = function
  | ' '
  | '\r'
  | '\n'
  | '\t' ->
      true
  | _ -> false
;;

let is_alpha_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_alpha = function
  | 'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_underscore = function
  | '_' -> true
  | _ -> false
;;

let ws = A.take_while is_whitespace
let ws1 = A.take_while1 is_whitespace
let alpha_lo1 = A.take_while1 is_alpha_lower

let ident =
  let alpha_under c = is_alpha c || is_underscore c in

  A.satisfy is_alpha_lower >>= fun first ->
  A.take_while alpha_under >>= fun rest ->
  A.return
    @@ (String.make 1 first) ^ rest
[@@ocamlformat "disable"]

let words = A.sep_by (A.char ' ') alpha_lo1
let colon = A.char ':'

let parse_record_attr : record_attr A.t =
  ws    *> ident <* ws1       >>= fun key ->
  colon *> ws *> words <* ws1 >>= fun values ->
  A.return
    @@ Record_attr (key, values)
[@@ocamlformat "disable"]

let type_ = A.string "type"
let eq = A.char '='
let curly_l = A.char '{'
let curly_r = A.char '}'

let parse_record : record A.t =
  let attrs =
    A.sep_by (A.char ';') parse_record_attr
  in
  ws *> type_   *> ws1 *> alpha_lo1 <* ws  <* eq            >>= fun type_name ->
  ws *> curly_l *> ws  *> attrs     <* ws  <* curly_r <* ws >>= fun attrs ->

  A.return
    @@ Record (type_name, attrs)
[@@ocamlformat "disable"]

let make_parser parser = A.parse_string ~consume:All parser

let%test_unit "parsing an OCaml record" =
  let ( => ) = [%test_eq: (record, string) Result.t] in
  let parse = make_parser parse_record in
  let valid_input =
    {|

    type customer =
      { id : int
      ; name : string
      ; is_active : bool
      }

  |}
  in
  let invalid_input =
    {|

    type customer =
      { id : int
      , name : string
      }

  |}
  in
  let want : record =
    Record
      ("customer"
      , [ Record_attr ("id", ["int"])
        ; Record_attr ("name", ["string"])
        ; Record_attr ("is_active", ["bool"])
        ]
      )
  in
  ()
  ; parse valid_input   => Ok want
  ; parse invalid_input => Error ": char '}'"
[@@ocamlformat "disable"]

let to_camel_case str : string =
  let words = String.split ~on:'_' str in
  let words = List.map ~f:String.capitalize words in
  String.concat ~sep:"" words |> String.uncapitalize
;;

let elm_conv = function
  | "unit" -> "()"
  | "option" -> "Maybe"
  | s -> String.capitalize s
;;

let elm_type (elems : string list) =
  List.map ~f:elm_conv elems |> List.rev |> String.concat ~sep:" "
;;

let elm_string_of_record (Record (name, lst)) : string =
  let string_of_attr (Record_attr (name, typ)) =
    sprintf "%s : %s" (to_camel_case name) (elm_type typ)
  in
  let attrs = List.map ~f:string_of_attr lst in
  sprintf
{|
type alias %s =
  { %s
  }
|}
    (String.capitalize name)
    (String.concat ~sep:"\n  , " attrs)
[@@ocamlformat "disable"]

let%test_unit "converting an OCaml record to an Elm record" =
  let ( => ) = [%test_eq: string] in
  let input : record =
    Record
      ("customer"
      , [ Record_attr ("id", ["int"])
        ; Record_attr ("alt_name", ["string"; "option"])
        ]
      )
  in
  let want =
{|
type alias Customer =
  { id : Int
  , altName : Maybe String
  }
|}
  in
  (elm_string_of_record input) => want
[@@ocamlformat "disable"]
