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

let fail_setup e = failwith @@ "test setup failed: " ^ Caqti_error.show e

let%test_unit "find_by_id" =
  let ( => ) = [%test_eq: (Base.string, Base.string) Base.Result.t] in
  let open Lwt_result.Syntax in
  let conn = Init.caqti_conn () in
  let setup : (unit, 'error) result Lwt.t =
    let* () = create_tbl conn () in
    let* () = insert conn "John" "Doe" in
    Lwt.return_ok ()
  in
  match Lwt_main.run setup with
  | Error e -> fail_setup e
  | Ok () ->
      let prom =
        let open Lwt.Syntax in
        let* res = find_by_id conn 1 in
        let res = Result.map_error Caqti_error.show res in
        Lwt.return res
      in
      Lwt_main.run prom => Ok "John"
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

let test_will_count conn =
  let open Lwt.Syntax in
  let* res = count conn () in
  let res = Result.map_error Caqti_error.show res in
  Lwt.return res
;;

let test_will_insert_then_count conn =
  let open Lwt.Syntax in
  let* ins_res = insert conn "Jane" "Doe" in
  let* cnt_res = count conn () in
  let res =
    Result.bind ins_res (fun () -> cnt_res) |> Result.map_error Caqti_error.show
  in
  Lwt.return res
;;

let init_test_db () =
  let conn = Init.caqti_conn () in
  let setup : (unit, 'error) result Lwt.t =
    let open Lwt_result.Syntax in
    let* () = create_tbl conn () in
    Lwt.return_ok ()
  in
  (conn, setup)
;;

let%test_unit "count returns 1 since, John already exists" =
  let ( => ) = [%test_eq: (Base.int, Base.string) Base.Result.t] in
  (* GIVEN *)
  let conn, setup = init_test_db () in
  match Lwt_main.run setup with
  | Error e -> fail_setup e
  | Ok () ->
      (* WHEN *)
      let prom = test_will_count conn in
      (* THEN *)
      Lwt_main.run prom => Ok 1
;;

let%test_unit "count returns 2, after inserting Jane" =
  let ( => ) = [%test_eq: (Base.int, Base.string) Base.Result.t] in
  (* GIVEN *)
  let conn, setup = init_test_db () in
  match Lwt_main.run setup with
  | Error e -> fail_setup e
  | Ok () ->
      (* WHEN *)
      let prom = test_will_insert_then_count conn in
      (* THEN *)
      Lwt_main.run prom => Ok 2
;;
