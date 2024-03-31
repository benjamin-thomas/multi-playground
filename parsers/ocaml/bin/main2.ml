[@@@ocamlformat "disable"]

open Printf
module A = Angstrom

(*

Parse a string to a function call, see:

  https://discuss.ocaml.org/t/is-there-example-code-for-parsing-function-call-expression-from-string/14397/4


---
VERBOSE VERSION
---

dune exec ./bin/main2.exe

*)

let ( >>= ) = A.( >>= )

type scaled = { side : string; amount : int; order_count : int }

type operation =
  | NoOp
  | Scaled of { side : string; amount : int; order_count : int }
[@@warning "-37"]

let dbl_quoted : string A.t =
  A.char '"' >>= fun _ ->
  A.take_while1 (fun c -> c <> '"') >>= fun str ->
  A.char '"' >>= fun _ -> A.return str
;;

let digit =
  A.take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
;;

let scaled_parser : scaled A.t =
  A.string "side="       >>= fun _ ->
  dbl_quoted             >>= fun side ->
  A.char ','             >>= fun _ ->
  A.string "amount="     >>= fun _ ->
  digit                  >>= fun amount ->
  A.char ','             >>= fun _ ->
  A.string "orderCount=" >>= fun _ ->
  digit                  >>= fun order_count ->
  let amount = int_of_string amount in
  let order_count = int_of_string order_count in
  A.return { side; amount; order_count }
;;

let operation_parser : operation A.t =
  A.string "scaled" >>= fun _ ->
  A.char '('        >>= fun _ ->
  scaled_parser     >>= fun params ->
  A.char ')'        >>= fun _ ->
  A.return
    (Scaled
       { side = params.side
       ; amount = params.amount
       ; order_count = params.order_count
       })
;;

let eval = function
  | NoOp -> printf "NoOp..."
  | Scaled { side; amount; order_count } ->
      printf "Ready to call internal scale fn with: (%s, %d, %d)\n" side amount
        order_count
;;

let () =
  let input = {|scaled(side="buy",amount=100,orderCount=30)|} in
  let parsed = A.parse_string ~consume:Prefix operation_parser input in
  match parsed with
  | Ok operation -> eval operation
  | Error err ->
      prerr_endline "I failed to parse the input :("
      ; prerr_endline "Maybe use `A.peek` to debug your parser."
      ; prerr_endline "---"
      ; prerr_endline err
