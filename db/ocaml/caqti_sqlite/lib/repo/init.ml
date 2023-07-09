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
         ( id          INTEGER PRIMARY KEY AUTOINCREMENT
         , first_name  TEXT NOT NULL CHECK (LENGTH(first_name) < 255)
         , middle_name TEXT     NULL CHECK (LENGTH(first_name) < 255)
         , last_name   TEXT NOT NULL CHECK (LENGTH(last_name)  < 255)
         )
    |}
  ;;

  let create_book_tbl =
    Caqti_type.(unit ->. unit)
      {|
       CREATE TABLE book
         ( id               INTEGER PRIMARY KEY AUTOINCREMENT
         , title            TEXT    NOT NULL UNIQUE CHECK (LENGTH(title) < 255)
         , back_cover_descr TEXT        NULL
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
  (* NOTE: using a `RETURNING id` clause requires sqlite >= v3.35, ok on Ubuntu 22.04, not ok on Ubuntu 20.04 *)
  let add_data () =
    let add_author = Author.insert' conn in
    let add_book = Book.insert' conn in
    let add_bibli = Bibliography.insert conn in
    let open Lwt_result.Syntax in
    (*
        John Whitington
      *)
    let* john_whitigton =
      add_author
        { first_name = "John"; middle_name = None; last_name = "Whitington" }
    in
    let* ocaml_ftvb = add_book { title = "OCaml from the Very Beginning" } in
    let* more_ocaml = add_book { title = "More OCaml" } in
    let* () = add_bibli { author_id = john_whitigton; book_id = ocaml_ftvb } in
    let* () = add_bibli { author_id = john_whitigton; book_id = more_ocaml } in

    (*
        Graham Hutton
      *)
    let* graham_hutton =
      add_author
        { first_name = "Graham"; middle_name = None; last_name = "Hutton" }
    in
    let* prog_with_haskell = add_book { title = "Programming in Haskell" } in
    let* () =
      add_bibli { author_id = graham_hutton; book_id = prog_with_haskell }
    in

    (* Anil Madhavapeddy and Yaron Minsky *)
    let* anil_madhavapeddy =
      add_author
        { first_name = "Anil"; middle_name = None; last_name = "Madhavapeddy" }
    in
    let* yaron_minsky =
      add_author
        { first_name = "Yaron"; middle_name = None; last_name = "Minsky" }
    in
    let* rwo = add_book { title = "Real World OCaml" } in
    let* () = add_bibli { author_id = anil_madhavapeddy; book_id = rwo } in
    let* () = add_bibli { author_id = yaron_minsky; book_id = rwo } in

    Lwt.return_ok ()
  in
  transact conn add_data
;;

let add_author conn first_name last_name : (unit, 'error) result Lwt.t =
  Author.insert conn { first_name; middle_name = None; last_name }
;;

let seed2 conn : (unit, 'error) result Lwt.t =
  let ( let* ) = Lwt_result.bind in
  let* () = add_author conn "John" "Doe" in
  let* () = add_author conn "Jane" "Doe" in
  let* () = add_author conn "Robert" "Doe" in
  Lwt.return_ok ()
;;

let seed3 conn : (unit, 'error) result Lwt.t =
  let%lwt res = add_author conn "John" "Doe" in
  match res with
  | Error e -> Lwt.return_error e
  | Ok () -> Lwt.return_ok ()
;;

let seed4 conn : (unit, 'error) result Lwt.t =
  match%lwt add_author conn "John" "Doe" with
  | Error e -> Lwt.return_error e
  | Ok () -> Lwt.return_ok ()
;;

let bind fn then_ : (unit, 'error) result Lwt.t =
  match%lwt fn with
  | Error e -> Lwt.return_error e
  | Ok () -> then_ ()
;;

let seed5 conn : (unit, 'error) result Lwt.t =
  let ( >>= ) = bind in

  Lwt.return_ok ()             >>= fun () ->
  add_author conn "John" "Doe" >>= fun () ->
  add_author conn "Jane" "Doe" >>= fun () ->
  add_author conn "Robert" "Doe"
[@@ocamlformat "disable"]

let seed6 conn : (unit, 'error) result Lwt.t =
  let bind = Lwt_result.bind in
  let%monad () = add_author conn "John" "Doe" in
  let%m () = add_author conn "Jane" "Doe" in
  let%m () = add_author conn "Robert" "Doe" in
  Lwt.return_ok ()
;;

let seed7 conn : (unit, 'error) result Lwt.t =
  let bind = Lwt_result.bind in
  Lwt.return_ok ()
  ; %m add_author conn "John" "Doe"
  ; %m add_author conn "Jane" "Doe"
  ; %m add_author conn "Robert" "Doe"
  [@@ocamlformat "disable"]

let seed8 conn : (unit, 'error) result Lwt.t =
  let%lwt_res () = add_author conn "John" "Doe" in
  let%lwt_res () = add_author conn "Jane" "Doe" in
  let%lwt_res () = add_author conn "Robert" "Doe" in
  Lwt.return_ok ()
;;

let seed9 conn : (unit, 'error) result Lwt.t =
  Lwt.return_ok ()
  ; %lwt_res add_author conn "John" "Doe"
  ; %lwt_res add_author conn "Jane" "Doe"
  ; %lwt_res add_author conn "Robert" "Doe"
[@@ocamlformat "disable"]

let double n =
  let%opt x = n in
  Some (x * 2)
;;

let double_async =
  let%lwt_res x = Lwt.return_ok 1 in
  Lwt.return_ok (x * 2)
;;
