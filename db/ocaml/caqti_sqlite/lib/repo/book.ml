module Q = struct
  open Caqti_request.Infix

  (*
    Caqti infix operators

    ->! decodes a single row
    ->? decodes zero or one row
    ->* decodes many rows
    ->. expects no row
  *)

  let count = Caqti_type.(unit ->! int) {|
  SELECT COUNT(*) FROM books
  |}

  let insert =
    Caqti_type.(tup2 int string ->. unit)
      {|
  INSERT INTO books (author_id, title) VALUES (?, ?)
  |}
  ;;
end

type book = { author_id : int; title : string }

let count (module Conn : Caqti_lwt.CONNECTION) = Conn.find Q.count

let insert (module Conn : Caqti_lwt.CONNECTION) (b : book) =
  Conn.exec Q.insert (b.author_id, b.title)
;;
