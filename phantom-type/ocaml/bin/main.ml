(* dune exec ./bin/main.exe -w *)

open Printf

let () =
  ()
  ; print_newline ()
  ; print_newline ()
;;

(*
   Like with the Gleam example, since I must export the "validated password" type (from validate),
   this unfortunately gives the caller the ability to construct the type manually 🤔️
   let _pw : Authentication.validated Authentication.password = Password "wat"
*)

let () =
  let pw = Authentication.from_string "p@a$$w0rd" in
  match Authentication.validate pw with
  | Ok (Password str) -> printf "Password check OK: %s!\n%!" str
  | Error err ->
    fprintf stderr "Password rejected: %s\n%!" (Authentication.string_of_reason err)
;;
