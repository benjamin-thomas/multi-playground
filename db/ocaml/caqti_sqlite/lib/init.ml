(*
   "Unpack" the `Caqti_lwt.CONNECTION` module into `db`,
   by creating an anonymous `first-class module` with the `val` keyword (on the left).

   See: https://dev.realworldocaml.org/first-class-modules.html
  *)
let caqti_conn () =
  let caqti_conn =
    let cwd = Sys.getcwd () in
    let path = Printf.sprintf "sqlite3://%s/db.sqlite3" cwd in
    let promise_caqti_conn_result = Caqti_lwt.connect (Uri.of_string path) in
    let promise_caqti_conn =
      Lwt.bind promise_caqti_conn_result Caqti_lwt.or_fail
    in

    Lwt_main.run promise_caqti_conn
  in
  (module (val caqti_conn) : Caqti_lwt.CONNECTION)
;;
