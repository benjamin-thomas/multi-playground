module Q = struct
  open Caqti_request.Infix

  (*
    Caqti infix operators

    ->! decodes a single row
    ->? decodes zero or one row
    ->* decodes many rows
    ->. expects no row
  *)

  let create_tbl =
    Caqti_type.(unit ->. unit)
    @@ {|
   CREATE TABLE IF NOT EXISTS customers
     ( id INTEGER PRIMARY KEY AUTOINCREMENT
     , first_name VARCHAR(255) UNIQUE
     , last_name VARCHAR(255)
     )
   |}
  ;;

  let insert =
    Caqti_type.(tup2 string string ->. unit)
    @@ {|
  INSERT INTO customers (first_name, last_name)
  VALUES (?, ?)
    |}
  ;;

  let find_by_id =
    Caqti_type.(int ->! string)
    @@ {|
    SELECT first_name
    FROM customers
    WHERE id = ?
    |}
  ;;

  let update =
    Caqti_type.(tup3 int string string ->. unit)
    @@ {|
    UPDATE customers
      SET first_name =  ?
    , SET last_name = ?
    WHERE id = ?
    |}
  ;;

  let delete =
    Caqti_type.(int ->. unit)
    @@ {|
    DELETE FROM customers
    WHERE id = ?
    |}
  ;;

  let count =
    Caqti_type.(unit ->! int) @@ {|
    SELECT COUNT(*) FROM customers
    |}
  ;;
end

type customer = { first_name : string; last_name : string }

let create_tbl (module Conn : Caqti_lwt.CONNECTION) = Conn.exec Q.create_tbl

(*
   $ dune utop
   utop # open Repo;;
   utop # let conn = Init.caqti_conn ();;
   utop # Customer.insert conn "Robert" "Doe";;
 *)
let insert (module Conn : Caqti_lwt.CONNECTION) first_name last_name =
  Conn.exec Q.insert (first_name, last_name)
;;

let find_by_id (module Conn : Caqti_lwt.CONNECTION) id =
  Conn.find Q.find_by_id id
;;

let update (module Conn : Caqti_lwt.CONNECTION) id (c : customer) =
  Conn.exec Q.update (id, c.first_name, c.last_name)
;;

let delete (module Conn : Caqti_lwt.CONNECTION) id = Conn.exec Q.delete id

(*
   $ dune utop
   utop # open Repo;;
   utop # let conn = Init.caqti_conn ();;
   utop # Customer.count conn ();;
 *)
let count (module Conn : Caqti_lwt.CONNECTION) = Conn.find Q.count
