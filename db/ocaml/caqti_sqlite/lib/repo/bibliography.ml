module Row = struct
  type t =
    { book_id : int
    ; title : string
    ; first_name : string
    ; middle_name : string option
    ; last_name : string
    }
end

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

  let x = Caqti_type.(unit ->* tup2 (tup2 string string) string)

  (* Example showing how to go beyond tup4 (there is no tup5) *)
  let ls =
    Caqti_type.(
      unit
      ->* tup2 int (tup2 string (tup2 string (tup2 (option string) string))))
      {|
       SELECT x.id
            , b.title
            , a.first_name
            , a.middle_name
            , a.last_name
       FROM bibliography AS x

       INNER JOIN author AS a
               ON a.id = x.author_id

       INNER JOIN book AS b
               ON b.id = x.book_id
      |}
  ;;

  let ls' =
    Caqti_type.(unit ->* tup2 int string)
      {|
       SELECT x.id
            , b.title
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
let ls' (module Conn : Caqti_lwt.CONNECTION) = Conn.collect_list Q.ls'

let ls'' =
  [%rapper
    get_many
      {sql|

        SELECT @int{x.id}
             , @string{b.title}

       FROM bibliography AS x

       INNER JOIN author AS a
               ON a.id = x.author_id

       INNER JOIN book AS b
               ON b.id = x.book_id

        |sql}]
;;

let ls''' =
  [%rapper
    get_many
      {sql|

        SELECT @int{x.id}
             , @string{b.title}
             , @string{a.first_name}
             , @string?{a.middle_name}
             , @string{a.last_name}

       FROM bibliography AS x

       INNER JOIN author AS a
               ON a.id = x.author_id

       INNER JOIN book AS b
               ON b.id = x.book_id

        |sql}]
;;

open Sexplib.Std

type ls4_row =
  { id : int
  ; title : string
  ; first_name : string
  ; middle_name : string option
  ; last_name : string
  }
[@@deriving sexp, ord]

let ls4 =
  [%rapper
    get_many
      {sql|

        SELECT x.id          AS @int{id}
             , b.title       AS @string{title}
             , a.first_name  AS @string{first_name}
             , a.middle_name AS @string?{middle_name}
             , a.last_name   AS @string{last_name}

       FROM bibliography AS x

       INNER JOIN author AS a
               ON a.id = x.author_id

       INNER JOIN book AS b
               ON b.id = x.book_id

        |sql}
      record_out]
;;
