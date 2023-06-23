(* ./bin/setup.ml *)

module Queries = Lib__Queries

let ( let** ) promise fn =
  let ( let* ) = Lwt.bind in
  let* result = promise in
  match result with
  | Ok x -> fn x
  | Error err -> Lwt.return (Error err)
;;

let () =
  let log s = Printf.printf "[LOG] setup2: %s\n" s in

  let promise1 = Queries.create_customers_tbl () in
  let promise2 = Queries.create_products_tbl () in

  let all_promises : (unit, 'error) result Lwt.t =
    let** () = promise1 in
    let** () = promise2 in
    Lwt.return_ok ()
  in

  Lwt_main.run all_promises |> function
  | Ok () -> log "Success!"
  | Error _ -> log "Something went wrong :("
;;
