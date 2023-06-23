(* ./bin/setup.ml *)

module Queries = Lib__Queries

let ( >>=? ) promise fn =
  let ( >>= ) = Lwt.bind in
  promise >>= fun result ->
  match result with
  | Ok x -> fn x
  | Error err -> Lwt.return (Error err)
;;

let () =
  let log s = Printf.printf "[LOG] setup: %s\n" s in
  let promise1 = Queries.create_customers_tbl () in
  let promise2 = Queries.create_products_tbl () in

  let all_promises : (unit, 'error) result Lwt.t =
    promise1 >>=? fun () ->
    promise2 >>=? fun () ->
    Lwt.return_ok ()
  in

  Lwt_main.run all_promises |> function
  | Ok () -> log "Success!"
  | Error _ -> log "Something went wrong :("
[@@ocamlformat "disable"]
