(*
 See: https://discuss.ocaml.org/t/let-syntax-semicolon-and-chaining-units/12521/22
 *)

let () = Ppx_monad.register "lwt_res" ~monad:"Lwt_result"
