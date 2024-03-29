module Author = Repo.Author

let%test_unit "count returns 0, when there are no rows" =
  let ( => ) = [%test_eq: (Base.int, Base.string) Base.Result.t] in
  let conn, setup = Setup.fresh_db () in
  match Lwt_main.run setup with
  | Error e -> Setup.fail e
  | Ok () ->
      let prom =
        let open Lwt.Syntax in
        let* res = Author.count conn () in
        let res = Result.map_error Caqti_error.show res in
        Lwt.return res
      in
      Lwt_main.run prom => Ok 0
;;

let%test_unit "count returns 1, after inserting Jane" =
  let ( => ) = [%test_eq: (Base.int, Base.string) Base.Result.t] in
  let conn, setup = Setup.fresh_db () in
  match Lwt_main.run setup with
  | Error e -> Setup.fail e
  | Ok () ->
      let prom =
        let open Lwt.Syntax in
        let* ins_res =
          Author.insert conn
            { first_name = "Jane"; middle_name = None; last_name = "Doe" }
        in
        let* cnt_res = Author.count conn () in
        let res =
          Result.bind ins_res (fun () -> cnt_res)
          |> Result.map_error Caqti_error.show
        in
        Lwt.return res
      in
      Lwt_main.run prom => Ok 1
;;

let%test_unit "find_by_id" =
  let ( => ) = [%test_eq: (Base.string, Base.string) Base.Result.t] in
  let conn, setup = Setup.fresh_db () in
  match Lwt_main.run setup with
  | Error e -> Setup.fail e
  | Ok () ->
      let prom =
        let open Lwt.Syntax in
        let* ins_res =
          Author.insert conn
            { first_name = "John"; middle_name = None; last_name = "Doe" }
        in
        let* find_res = Author.find_by_id conn 1 in
        let res =
          Result.bind ins_res (fun () -> find_res)
          |> Result.map_error Caqti_error.show
        in
        Lwt.return res
      in
      Lwt_main.run prom => Ok "John"
;;

let%test_unit "read many" =
  let ( => ) = [%test_eq: (Base.string Base.list, Base.string) Base.Result.t] in
  let conn, setup = Setup.fresh_db () in
  match Lwt_main.run setup with
  | Error e -> Setup.fail e
  | Ok () ->
      let prom =
        let res =
          let open Lwt_result.Syntax in
          let prom =
            let* () =
              Author.insert conn
                { first_name = "John"; middle_name = None; last_name = "Doe" }
            in
            let* () =
              Author.insert conn
                { first_name = "Jane"; middle_name = None; last_name = "Doe" }
            in
            let* found = Author.ls conn () in
            Lwt.return_ok found
          in
          Lwt_main.run prom |> Result.map_error Caqti_error.show
        in
        Lwt.return res
      in
      Lwt_main.run prom => Ok [ "John"; "Jane" ]
;;
