module A = Angstrom
module B = Base

let ( >>= ) = A.( >>= )
let ( >>| ) = A.( >>| )

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

let skip_ws = A.skip_while is_whitespace
let alpha_lower = A.take_while1 is_alpha_lower

let ident =
  A.satisfy is_alpha_lower >>= fun first ->
  A.take_while1(function
  | 'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | '_' -> true
  | _ -> false
) >>= fun rest ->
  A.return ((String.make 1 first) ^ rest)
[@@ocamlformat "disable"]

let parse_record_attr : record_attr A.t =
  let parse_vals =
    A.sep_by (A.char ' ') alpha_lower
  in
  skip_ws     >>= fun () ->
  ident       >>= fun key ->
  skip_ws     >>= fun () ->
  A.char ':'  >>= fun _ ->
  skip_ws     >>= fun () ->
  parse_vals  >>= fun vals ->
  skip_ws     >>= fun () ->
  A.return @@ Record_attr (key, vals)
[@@ocamlformat "disable"]

let parse_record : record A.t =
  let parse_attrs =
    A.sep_by (A.char ';') parse_record_attr
  in
  skip_ws           >>= fun () ->
  A.string "type"   >>= fun _ ->
  skip_ws           >>= fun () ->
  alpha_lower       >>= fun type_name ->
  skip_ws           >>= fun () ->
  A.char '='        >>= fun _ ->
  skip_ws           >>= fun () ->
  A.char '{'        >>= fun _ ->
  skip_ws           >>= fun () ->
  parse_attrs >>= fun attrs ->
  skip_ws           >>= fun () ->
  A.char '}'        >>= fun _ ->
  skip_ws           >>= fun () ->
  A.return @@ Record (type_name, attrs)
[@@ocamlformat "disable"]

let make_parser parser = A.parse_string ~consume:All parser

let make_parser' parser_name parser input =
  let parse = A.parse_string ~consume:All parser in
  parse input |> Result.map_error ~f:(fun _ -> parser_name)
;;

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
  ; parse valid_input => Ok want
  ; parse invalid_input => Error ": char '}'"
[@@ocamlformat "disable"]

let to_camel_case (str : string) : string =
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

let to_elm (Record (name, lst)) : string =
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
  ()
  ; (to_elm input) => want
[@@ocamlformat "disable"]
