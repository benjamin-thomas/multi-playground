(*
   "Unpack" the `Caqti_lwt.CONNECTION` into a module, by creating an anonymous
    `first-class module` with the `val` keyword (on the left).

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

  let create_author_tbl =
    Caqti_type.(unit ->. unit)
      {|
       CREATE TABLE author
         ( id         INTEGER PRIMARY KEY AUTOINCREMENT
         , first_name TEXT NOT NULL CHECK (LENGTH(first_name) < 255)
         , last_name  TEXT NOT NULL CHECK (LENGTH(last_name)  < 255)
         )
    |}
  ;;

  let create_book_tbl =
    Caqti_type.(unit ->. unit)
      {|
       CREATE TABLE book
         ( id               INTEGER PRIMARY KEY AUTOINCREMENT
         , title            TEXT NOT NULL UNIQUE CHECK (LENGTH(title) < 255)
         , back_cover_descr TEXT NULL
         )
    |}
  ;;

  let create_bibliography_tbl =
    Caqti_type.(unit ->. unit)
      {|
       CREATE TABLE bibliography
         ( id        INTEGER PRIMARY KEY AUTOINCREMENT
         , book_id   INTEGER NOT NULL REFERENCES book(id)
         , author_id INTEGER NOT NULL REFERENCES author(id)
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
  let* () = Conn.exec Q.create_author_tbl () in
  let* () = Conn.exec Q.create_book_tbl () in
  let* () = Conn.exec Q.create_bibliography_tbl () in
  let* () = Conn.commit () in
  Lwt.return_ok ()
;;

let transact (module Conn : Caqti_lwt.CONNECTION) fn =
  let open Lwt_result.Syntax in
  let* () = Conn.start () in
  let* () = fn () in
  let* () = Conn.commit () in
  Lwt.return_ok ()
;;

(*
   $ dune utop
   utop # open Repo;;
   utop # let conn = Init.caqti_conn ();;
   utop # Init.create_tables conn;;
   utop # Init.seed conn;;
   utop # Bibliography.ls conn ();;
 *)

let seed conn =
  transact conn @@ fun () ->
  let open Lwt_result.Syntax in
  (* John Wihtington, author_id=1 *)
  let* () =
    (* Use a `RETURNING id` clause instead, once sqlite > v3.35 becomes widely available *)
    Author.insert conn { first_name = "John"; last_name = "Whitington" }
  in
  let* () = Book.insert conn { title = "OCaml from the Very Beginning" } in
  let* () = Book.insert conn { title = "More OCaml" } in
  let* () = Bibliography.insert conn { author_id = 1; book_id = 1 } in
  let* () = Bibliography.insert conn { author_id = 1; book_id = 2 } in

  (* Graham Hutton, id=2 *)
  let* () =
    Author.insert conn { first_name = "Graham"; last_name = "Hutton" }
  in
  let* () = Book.insert conn { title = "Programming in Haskell" } in
  let* () = Bibliography.insert conn { author_id = 2; book_id = 3 } in

  (* Anil Madhavapeddy, id=3 ; Yaron Minsky, id=4 *)
  let* () =
    Author.insert conn { first_name = "Anil"; last_name = "Madhavapeddy" }
  in
  let* () = Author.insert conn { first_name = "Yaron"; last_name = "Minsky" } in
  let* () = Book.insert conn { title = "Real World OCaml" } in
  let* () = Bibliography.insert conn { author_id = 3; book_id = 4 } in
  let* () = Bibliography.insert conn { author_id = 4; book_id = 4 } in
  Lwt.return_ok ()
;;
