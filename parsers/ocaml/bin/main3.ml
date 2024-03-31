[@@@ocamlformat "disable"]

open Printf

module A = Angstrom

(*

Parse a string to a function call, see:

  https://discuss.ocaml.org/t/is-there-example-code-for-parsing-function-call-expression-from-string/14397/4


---
CONSENSED VERSION
---

dune exec ./bin/main2.exe

*)


let ( *> ) = A.( *> )
let ( <* ) = A.( <* )
let ( <*> ) = A.( <*> )
let ( <$> ) = A.( <$> )
let ( >>| ) = A.( >>| )
let ( <?> ) = A.(<?>)


type operation =
  | NoOp
  | Scaled of { side: string
              ; amount: int
              ; order_count: int
              }
  [@@warning "-37"]

let str : string A.t =
  A.char '"'
  *> A.take_while1 (fun c -> c <> '"') <*
  A.char '"'

let digits : string A.t =
  A.take_while1 @@ function
    | '0' .. '9' -> true
    | _ -> false

let int : int A.t = digits >>| int_of_string

let ws : unit A.t =
  A.skip_while @@ function
    | ' ' | '\t' -> true
    | _ -> false

let comma : char A.t = A.char ','

let sep : unit A.t = ws <* comma <* ws

let make_scaled side amount order_count : operation =
  Scaled { side
         ; amount
         ; order_count
         }

let _scaled_parser : operation A.t =
  make_scaled
  <$> (A.string "side="       *> str <* sep)
  <*> (A.string "amount="     *> int <* sep)
  <*> (A.string "orderCount=" *> int       )

(* <?> is optional and for error handling  *)
let scaled_parser : operation A.t =
  make_scaled
  <$> (A.string "side="       *> str <* sep) <?> "side"
  <*> (A.string "amount="     *> int <* sep) <?> "amount"
  <*> (A.string "orderCount=" *> int       ) <?> "orderCount"
  <?> "scaled"


let _operation_parser : operation A.t =
  (Fun.id)
  <$> (A.string "scaled" *> A.char '(' *> scaled_parser <* A.char ')')

let _operation_parser : operation A.t =
  Fun.id
  <$> (A.string "scaled" *> A.char '(' *> scaled_parser <* A.char ')')

let _operation_parser : operation A.t =
  A.string "scaled" *> A.char '(' *> scaled_parser <* A.char ')'
  >>| Fun.id

let operation_parser : operation A.t =
  A.string "scaled" *> A.char '(' *> scaled_parser <* A.char ')'

let eval = function
  | NoOp -> printf "NoOp..."
  | Scaled {side; amount; order_count} ->
      printf
        "Ready to call internal scale fn with: (%s, %d, %d)\n"
        side amount order_count

let () =
let input = {|scaled(side="buy", amount=100, orderCount=30)|} in
let parsed = A.parse_string
               ~consume:Prefix
               operation_parser
               input
in
match parsed with
| Ok operation ->
    eval operation
| Error err ->
    prerr_endline "I failed to parse the input :(";
    prerr_endline "Maybe use `A.peek` to debug your parser.";
    prerr_endline "---";
    prerr_endline err
