let fail e = failwith @@ "test setup failed: " ^ Caqti_error.show e

let fresh_db () =
  let path = Sys.getcwd () ^ "/db.sqlite3" in
  let () = if Sys.file_exists path then Sys.remove path in

  let conn = Repo.Init.caqti_conn () in
  let setup : (unit, 'error) result Lwt.t =
    let open Lwt_result.Syntax in
    let* () = Repo.Init.create_tables conn in
    Lwt.return_ok ()
  in
  (conn, setup)
;;
