(* module Row = struct
     type t =
       { book_id : int; title : string; first_name : string; last_name : string }
   end *)

module Q = struct
  open Caqti_request.Infix
  (* open Caqti_type.Std

     let row =
       let encode Row.{ book_id; title; first_name; last_name } =
         Ok (book_id, title, first_name, last_name)
       in
       let decode (book_id, title, first_name, last_name) =
         Ok Row.{ book_id; title; first_name; last_name }
       in
       let rep = Caqti_type.(tup4 int string string string) in
       custom ~encode ~decode rep *)

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

  let ls =
    Caqti_type.(unit ->* tup4 int string string string)
      {|
       SELECT x.id
            , b.title
            , a.first_name
            , a.last_name
       FROM bibliography AS x

       INNER JOIN author AS a
               ON a.id = x.author_id

       INNER JOIN book AS b
               ON b.id = x.book_id
      |}
  ;;
end

type bibliography = { author_id : int; book_id : int }

let insert (module Conn : Caqti_lwt.CONNECTION) (b : bibliography) =
  Conn.exec Q.insert (b.author_id, b.book_id)
;;

(*
   $ dune utop
   utop # open Repo;;
   utop # let conn = Init.caqti_conn ();;
   utop # Bibliography.ls conn ();;
 *)
let ls (module Conn : Caqti_lwt.CONNECTION) = Conn.collect_list Q.ls
