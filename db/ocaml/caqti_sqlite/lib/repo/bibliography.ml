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
    Caqti_type.(tup2 int int ->. unit)
      {|
       INSERT INTO bibliography (author_id, book_id) VALUES (?, ?)
      |}
  ;;
end

type bibliography = { author_id : int; book_id : int }

let insert (module Conn : Caqti_lwt.CONNECTION) (b : bibliography) =
  Conn.exec Q.insert (b.author_id, b.book_id)
;;
