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

module Q = struct
  open Caqti_request.Infix

  let create_authors_tbl =
    Caqti_type.(unit ->. unit)
      {|
       CREATE TABLE authors
         ( id INTEGER PRIMARY KEY AUTOINCREMENT
         , first_name VARCHAR(255) UNIQUE
         , last_name VARCHAR(255)
         )
    |}
  ;;

  let create_books_tbl =
    Caqti_type.(unit ->. unit)
      {|
       CREATE TABLE books
         ( id INTEGER PRIMARY KEY AUTOINCREMENT
         , author_id NOT NULL REFERENCES authors(id)
         , title VARCHAR(255) UNIQUE
         , last_name VARCHAR(255)
         )
    |}
  ;;
end

(*
   $ dune utop
   utop # open Repo;;
   utop # let conn = Init.caqti_conn ();;
   utop # Init.create_tables conn;;
 *)

let create_tables (module Conn : Caqti_lwt.CONNECTION) =
  let open Lwt_result.Syntax in
  let* () = Conn.start () in
  let* () = Conn.exec Q.create_authors_tbl () in
  let* () = Conn.exec Q.create_books_tbl () in
  let* () = Conn.commit () in
  Lwt.return_ok ()
;;
