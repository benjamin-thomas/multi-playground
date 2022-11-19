(*
  rg --files | entr -c dune exec --display=quiet caqti_demo

  Disables warning-as-errors
    rg --files | entr -c dune exec --display=quiet --profile release caqti_demo
  Or append this to the dune file:
    (env (dev (flags (:standard -warn-error -A)))

  Tutorial at:
    https://medium.com/@bobbypriambodo/interfacing-ocaml-and-postgresql-with-caqti-a92515bdaa11
*)

let () = Lwt_main.run (Lib.Customer.fetchAll ())

(* This is the connection pool we will use for executing DB operations. *)
