(*
 * DB SETUP
 *)
let cwd = Sys.getcwd ()
let db_path = Printf.sprintf "sqlite3://%s/db.sqlite3" cwd

module Db : Caqti_lwt.CONNECTION =
  (val let ( >>= ) = Lwt.bind in

       Lwt_main.run
         ( Caqti_lwt.connect (Uri.of_string db_path) >>= fun res ->
           Caqti_lwt.or_fail res ))

(*
 * QUERIES
 *)
open Caqti_request.Infix

(*
    Caqti infix operators

    ->! decodes a single row
    ->? decodes zero or one row
    ->* decodes many rows
    ->. expects no row


  *)

(*
  Lib__Queries.create_customers_tbl ();;

  $ dune exec ./bin/setup.exe
 *)
let create_customers_tbl =
  let qry =
    Caqti_type.(unit ->. unit)
    @@ {|
 CREATE TABLE IF NOT EXISTS customers
   ( id INTEGER PRIMARY KEY AUTOINCREMENT
   , first_name VARCHAR(255)
   , last_name VARCHAR(255)
   )
 |}
  in
  Db.exec qry
;;

let create_products_tbl =
  let qry =
    Caqti_type.(unit ->. unit)
    @@ {|
 CREATE TABLE IF NOT EXISTS products
   ( id INTEGER PRIMARY KEY AUTOINCREMENT
   , name VARCHAR(255)
   )
 |}
  in
  Db.exec qry
;;

(*
   Lib__Queries.insert_customer { id = None; first_name = "John"; last_name = "Doe"};;
 *)
let insert_customer (c : Domain.customer) =
  let qry =
    Caqti_type.(tup2 string string ->. unit)
    @@ {|
 INSERT INTO customers (first_name, last_name)
 VALUES (?, ?)
   |}
  in
  Db.exec qry (c.first_name, c.last_name)
;;
