module A = Angstrom

(*
open Internal.Angstrom_tutorial;; open Angstrom;;
Angstrom.parse_string ~consume:Prefix number "123.0";;
Angstrom.parse_string ~consume:All kv "apples -23.48";;
 *)

(* let%test "valid" = Some { year = 2023; month = 5; day = 26 } = date_of_string "2023.05.27" *)

let is_whitespace = function
  | '\x20'
  | '\x0a'
  | '\x0d'
  | '\x09' ->
      true
  | _ -> false
;;

let whitespace = A.take_while1 is_whitespace
let ( >>= ) = A.( >>= )
let ( >>| ) = A.( >>| )
let ( <* ) = A.( <* )

let only_whitespace =
  whitespace >>= fun parsed_ws ->
  A.peek_char >>= function
  | None -> A.return parsed_ws
  | Some c -> A.fail (Format.sprintf "Unexpected char %c" c)
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let sign =
  A.peek_char >>= function
  | Some '-' -> A.advance 1 >>| fun () -> "-"
  | Some '+' -> A.advance 1 >>| fun () -> "+"
  | Some c when is_digit c -> A.return "+"
  | _ -> A.fail "Sign or digit expected"
;;

let dot =
  A.peek_char >>= function
  | Some '.' -> A.advance 1 >>| fun () -> true
  | _ -> A.return false
;;

let number =
  sign >>= fun sign ->
  A.take_while1 is_digit >>= fun whole ->
  dot >>= function
  | false -> A.return (float_of_string (sign ^ whole))
  | true ->
      A.take_while1 is_digit >>= fun part ->
      A.return (float_of_string (sign ^ whole ^ "." ^ part))
;;

let key =
  A.take_while1 (function
    | 'a' .. 'z' -> true
    | _ -> false)
;;

let kv =
  key <* whitespace >>= fun k ->
  number >>= fun v -> A.return (k, v)
;;

let integer = A.take_while1 is_digit
let integer2 = A.take_while is_digit

type parse_res =
  { all : (string, string) result; prefix : (string, string) result }

let parse parser str =
  { all = A.parse_string ~consume:All parser str
  ; prefix = A.parse_string ~consume:Prefix parser str
  }
;;

let parse_ws = parse whitespace
let parse_ws_only = parse only_whitespace
