module Init = Repo.Init
module Customer = Repo.Customer

let fail_setup e = failwith @@ "test setup failed: " ^ Caqti_error.show e

let init_test_db () =
  let path = Sys.getcwd () ^ "/db.sqlite3" in
  let () = if Sys.file_exists path then Sys.remove path in

  let conn = Init.caqti_conn () in
  let setup : (unit, 'error) result Lwt.t =
    let open Lwt_result.Syntax in
    let* () = Customer.create_tbl conn () in
    Lwt.return_ok ()
  in
  (conn, setup)
;;

let%test_unit "find_by_id" =
  let ( => ) = [%test_eq: (Base.string, Base.string) Base.Result.t] in
  let conn, setup = init_test_db () in
  match Lwt_main.run setup with
  | Error e -> fail_setup e
  | Ok () ->
      let prom =
        let open Lwt.Syntax in
        let* ins_res = Customer.insert conn "John" "Doe" in
        let* find_res = Customer.find_by_id conn 1 in
        let res =
          Result.bind ins_res (fun () -> find_res)
          |> Result.map_error Caqti_error.show
        in
        Lwt.return res
      in
      Lwt_main.run prom => Ok "John"
;;

let%test_unit "count returns 0, when there are no rows" =
  let ( => ) = [%test_eq: (Base.int, Base.string) Base.Result.t] in
  let conn, setup = init_test_db () in
  match Lwt_main.run setup with
  | Error e -> fail_setup e
  | Ok () ->
      let prom =
        let open Lwt.Syntax in
        let* res = Customer.count conn () in
        let res = Result.map_error Caqti_error.show res in
        Lwt.return res
      in
      Lwt_main.run prom => Ok 0
;;

let%test_unit "count returns 1, after inserting Jane" =
  let ( => ) = [%test_eq: (Base.int, Base.string) Base.Result.t] in
  let conn, setup = init_test_db () in
  match Lwt_main.run setup with
  | Error e -> fail_setup e
  | Ok () ->
      let prom =
        let open Lwt.Syntax in
        let* ins_res = Customer.insert conn "Jane" "Doe" in
        let* cnt_res = Customer.count conn () in
        let res =
          Result.bind ins_res (fun () -> cnt_res)
          |> Result.map_error Caqti_error.show
        in
        Lwt.return res
      in
      Lwt_main.run prom => Ok 1
;;
