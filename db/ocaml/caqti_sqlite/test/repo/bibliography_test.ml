module Bibliography = Repo.Bibliography

let%test_unit "use seed, then read many rows" =
  let ( => ) =
    [%test_eq:
      ( (Base.int * Base.string * Base.string * Base.string) Base.list
      , Base.string )
      Base.Result.t]
  in
  let conn, setup = Setup.fresh_db () in
  match Lwt_main.run setup with
  | Error e -> Setup.fail e
  | Ok () ->
      let prom =
        let open Lwt_result.Syntax in
        let* () = Repo.Init.seed conn in
        let* found = Bibliography.ls conn () in
        Lwt.return_ok found
      in
      let ran = Lwt_main.run prom |> Result.map_error Caqti_error.show in
      ran
      => Ok
           [ (1, "OCaml from the Very Beginning", "John", "Whitington")
           ; (2, "More OCaml", "John", "Whitington")
           ; (3, "Programming in Haskell", "Graham", "Hutton")
           ; (4, "Real World OCaml", "Anil", "Madhavapeddy")
           ; (5, "Real World OCaml", "Yaron", "Minsky")
           ]
;;