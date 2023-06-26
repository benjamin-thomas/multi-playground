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
     , first_name VARCHAR(255)
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
end

type customer = { first_name : string; last_name : string }

let create_tbl (module Conn : Caqti_lwt.CONNECTION) = Conn.exec Q.create_tbl

(*
   open Repo;;
   let conn = Init.caqti_conn ();;
   Customer.insert conn "Robert" "Doe";;
 *)
let insert (module Conn : Caqti_lwt.CONNECTION) first_name last_name =
  Conn.exec Q.insert (first_name, last_name)
;;

let find_by_id (module Conn : Caqti_lwt.CONNECTION) id =
  Conn.find Q.find_by_id id
;;

let find_by_id' (module Conn : Caqti_lwt.CONNECTION) id =
  let open Lwt.Syntax in
  let* res = Conn.find Q.find_by_id id in
  Lwt.return (Result.map_error Caqti_error.show res)
;;

[@@@warning "-33-26"]

let%test_unit "find_by_id" =
  let ( => ) = [%test_eq: (Base.string, Base.string) Base.Result.t] in
  (* let open Lwt_result.Syntax in *)
  let open Lwt.Syntax in
  let conn = Init.caqti_conn () in
  (* let find_by_id = find_by_id' conn in *)
  let prom = find_by_id' conn 1 in
  match (Lwt_main.run prom ) with
  | Error _ -> failwith "wat"
  | Ok x ->
    (

      Ok x => Ok "1"
      )
  (* ()
  ; find_by_id 1  => Ok "John"
  ; find_by_id 1  => Ok "John2" *)
[@@ocamlformat "disable"]

(*
 dune runtest -w

 FIXME: db is read in a _build/.sandbox/{RANDOM} folder
 *)
let%test_unit "find_by_id2" =
  let ( => ) = [%test_eq: (Base.string, Base.string) Base.Result.t] in
  let open Lwt_result.Syntax in
  let conn = Init.caqti_conn () in
  (* let find_by_id = find_by_id' conn in *)
  let prom = find_by_id' conn 1 in
  Lwt_main.run prom => Ok "John"
  (* ()
  ; find_by_id 1  => Ok "John"
  ; find_by_id 1  => Ok "John2" *)
[@@ocamlformat "disable"]

let update (module Conn : Caqti_lwt.CONNECTION) id (c : customer) =
  Conn.exec Q.update (id, c.first_name, c.last_name)
;;

let delete (module Conn : Caqti_lwt.CONNECTION) id = Conn.exec Q.delete id
