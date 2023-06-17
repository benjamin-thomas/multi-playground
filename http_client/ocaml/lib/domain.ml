(* == DOMAIN == *)

type delivered =
  { on    : (Ptime.t * int option)
  ; label : string
  }
[@@ocamlformat "disable"]

type delivery_status =
  { shipping_status : int
  ; delivered       : delivered option
  }
[@@ocamlformat "disable"]

let delivered_code = "DI1"
