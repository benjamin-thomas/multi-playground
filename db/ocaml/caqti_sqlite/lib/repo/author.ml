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
    Caqti_type.(tup3 string (option string) string ->. unit)
      {|
       INSERT INTO author (first_name, middle_name, last_name)
       VALUES (?, ?, ?)
      |}
  ;;

  let insert' =
    Caqti_type.(tup2 string string ->! int)
      {|
       INSERT INTO author (first_name, last_name)
       VALUES (?, ?) RETURNING id
      |}
  ;;

  let find_by_id =
    Caqti_type.(int ->! string)
      {|
       SELECT first_name
       FROM author
       WHERE id = ?
      |}
  ;;

  let ls =
    Caqti_type.(unit ->* string)
      {|
       SELECT first_name
       FROM author
      |}
  ;;

  let update =
    Caqti_type.(tup3 int string string ->. unit)
      {|
       UPDATE author
         SET first_name =  ?
       , SET last_name  = ?
       WHERE id = ?
      |}
  ;;

  let delete =
    Caqti_type.(int ->. unit)
      {|
       DELETE FROM author
       WHERE id = ?
      |}
  ;;

  let count =
    Caqti_type.(unit ->! int) {|
    SELECT COUNT(*) FROM author
    |}
  ;;
end

type author =
  { first_name : string; middle_name : string option; last_name : string }

(*
   $ dune utop
   utop # open Repo;;
   utop # let conn = Init.caqti_conn ();;
   utop # Author.insert conn "Robert" "Doe" "robert@example.com";;
 *)
let insert (module Conn : Caqti_lwt.CONNECTION) (a : author) =
  Conn.exec Q.insert (a.first_name, a.middle_name, a.last_name)
;;

let insert' (module Conn : Caqti_lwt.CONNECTION) (a : author) =
  Conn.find Q.insert' (a.first_name, a.last_name)
;;

let find_by_id (module Conn : Caqti_lwt.CONNECTION) id =
  Conn.find Q.find_by_id id
;;

let ls (module Conn : Caqti_lwt.CONNECTION) = Conn.collect_list Q.ls

let update (module Conn : Caqti_lwt.CONNECTION) id (a : author) =
  Conn.exec Q.update (id, a.first_name, a.last_name)
;;

let delete (module Conn : Caqti_lwt.CONNECTION) id = Conn.exec Q.delete id

(*
   $ dune utop
   utop # open Repo;;
   utop # let conn = Init.caqti_conn ();;
   utop # Customer.count conn ();;
 *)
let count (module Conn : Caqti_lwt.CONNECTION) = Conn.find Q.count
