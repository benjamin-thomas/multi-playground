module Q = struct
  open Caqti_request.Infix

  (*
    Caqti infix operators

    ->! decodes a single row
    ->? decodes zero or one row
    ->* decodes many rows
    ->. expects no row
  *)

  let insert =
    Caqti_type.(tup2 string string ->. unit)
      {|
       INSERT INTO authors (first_name, last_name)
       VALUES (?, ?)
    |}
  ;;

  let find_by_id =
    Caqti_type.(int ->! string)
      {|
       SELECT first_name
       FROM authors
       WHERE id = ?
      |}
  ;;

  let update =
    Caqti_type.(tup3 int string string ->. unit)
      {|
       UPDATE authors
         SET first_name =  ?
       , SET last_name = ?
       WHERE id = ?
      |}
  ;;

  let delete =
    Caqti_type.(int ->. unit)
      {|
       DELETE FROM authors
       WHERE id = ?
    |}
  ;;

  let count =
    Caqti_type.(unit ->! int) {|
    SELECT COUNT(*) FROM authors
    |}
  ;;
end

type customer = { first_name : string; last_name : string }

(*
   $ dune utop
   utop # open Repo;;
   utop # let conn = Init.caqti_conn ();;
   utop # Author.insert conn "Robert" "Doe" "robert@example.com";;
 *)
let insert (module Conn : Caqti_lwt.CONNECTION) (c : customer) =
  Conn.exec Q.insert (c.first_name, c.last_name)
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
