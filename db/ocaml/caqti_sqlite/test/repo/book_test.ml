module Author = Repo.Author
module Book = Repo.Book

let%test_unit "count returns 0, when there are no rows" =
  let ( => ) = [%test_eq: (Base.int, Base.string) Base.Result.t] in
  let conn, setup = Setup.fresh_db () in
  match Lwt_main.run setup with
  | Error e -> Setup.fail e
  | Ok () ->
      let prom =
        let open Lwt.Syntax in
        let* res = Book.count conn () in
        let res = Result.map_error Caqti_error.show res in
        Lwt.return res
      in
      Lwt_main.run prom => Ok 0
;;

let%test_unit "count returns 1, after inserting OFTVB" =
  let ( => ) = [%test_eq: (Base.int, Base.string) Base.Result.t] in
  let conn, setup = Setup.fresh_db () in
  match Lwt_main.run setup with
  | Error e -> Setup.fail e
  | Ok () ->
      let prom =
        let res =
          let open Lwt_result.Syntax in
          let prom =
            (* let* () =
                 (* Use a `RETURNING id` clause instead, once sqlite > v3.35 becomes widely available *)
                 Author.insert conn
                   { first_name = "John"; last_name = "Whitington" }
               in *)
            let* () =
              Book.insert conn { title = "OCaml from the Very Beginning" }
            in
            let* n = Book.count conn () in
            Lwt.return_ok n
          in
          Lwt_main.run prom |> Result.map_error Caqti_error.show
        in

        Lwt.return res
      in
      Lwt_main.run prom => Ok 1
;;
