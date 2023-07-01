module Author = Repo.Author
module Book = Repo.Book

let%test_unit "count returns 0, when there are no rows" =
  let ( => ) = [%test_eq: (Base.int, Base.string) Base.Result.t] in
  let conn, setup = Setup.init_db () in
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
  let conn, setup = Setup.init_db () in
  match Lwt_main.run setup with
  | Error e -> Setup.fail e
  | Ok () ->
      let prom =
        let open Lwt.Syntax in
        let* auth_ins_res =
          (* FIXME: returning id may be desired here! *)
          Author.insert conn { first_name = "Jane"; last_name = "Doe" }
        in
        let* ins_res =
          Book.insert conn
            { author_id = 1; title = "OCaml from the Very Beginning" }
        in
        let* cnt_res = Book.count conn () in
        let res =
          let ( let* ) = Result.bind in
          Result.map_error Caqti_error.show
          @@ let* () = auth_ins_res in
             let* () = ins_res in
             let* x = cnt_res in
             Ok x
        in
        Lwt.return res
      in
      Lwt_main.run prom => Ok 1
;;
