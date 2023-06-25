(*
 * rm ./db.sqlite3;dune exec ./bin/setup.exe
 *)

module Customer_repo = Repo.Customer
module Init = Repo.Init

(*
 * UTILS
 *)
let info_log fmt = Printf.printf ("[INFO] " ^^ fmt ^^ "\n%!")
let err_log fmt = Printf.printf ("[ERROR] " ^^ fmt ^^ "\n%!")

(*
 * BOOTSTRAP
 *)
let () =
  let open Lwt_result.Syntax in
  let conn = Init.caqti_conn () in
  let all_promises : (unit, 'error) result Lwt.t =
    let* () = Customer_repo.create_tbl conn () in
    let* () = Customer_repo.insert conn "John" "Doe" in
    let* () = Customer_repo.insert conn "Jane" "Doe" in
    Lwt.return_ok ()
  in

  Lwt_main.run all_promises |> function
  | Ok () -> info_log "Setup OK!"
  | Error e -> err_log "%s" (Caqti_error.show e)
;;
