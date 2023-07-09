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
        (* Example showing how to go beyond tup4 (there is no tup5) *)
        let found =
          List.map
            (fun (id, (title, (first_name, (_middle_name, last_name)))) ->
              (id, title, first_name, last_name))
            found
        in
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

let%test_unit "testing ppx_monad" =
  let ( => ) =
    [%test_eq: ((Base.int * Base.string) Base.list, Base.string) Base.Result.t]
  in
  let conn, setup = Setup.fresh_db () in
  match Lwt_main.run setup with
  | Error e -> Setup.fail e
  | Ok () ->
      let prom =
        let open Lwt_result.Syntax in
        let* () = Repo.Init.seed5 conn in
        let* found = Repo.Author.ls' conn () in

        Lwt.return_ok found
      in
      let ran = Lwt_main.run prom |> Result.map_error Caqti_error.show in
      ran => Ok [ (1, "John"); (2, "Jane"); (3, "Robert") ]
;;

let%test_unit "testing simple, before ppx_rapper" =
  let ( => ) =
    [%test_eq: ((Base.int * Base.string) Base.list, Base.string) Base.Result.t]
  in
  let conn, setup = Setup.fresh_db () in
  match Lwt_main.run setup with
  | Error e -> Setup.fail e
  | Ok () ->
      let prom =
        let open Lwt_result.Syntax in
        let* () = Repo.Init.seed conn in
        let* found = Bibliography.ls' conn () in

        Lwt.return_ok found
      in
      let ran = Lwt_main.run prom |> Result.map_error Caqti_error.show in
      ran
      => Ok
           [ (1, "OCaml from the Very Beginning")
           ; (2, "More OCaml")
           ; (3, "Programming in Haskell")
           ; (4, "Real World OCaml")
           ; (5, "Real World OCaml")
           ]
;;

let%test_unit "testing equivalent, with ppx_rapper" =
  let ( => ) =
    [%test_eq: ((Base.int * Base.string) Base.list, Base.string) Base.Result.t]
  in
  let conn, setup = Setup.fresh_db () in
  match Lwt_main.run setup with
  | Error e -> Setup.fail e
  | Ok () ->
      let prom =
        let open Lwt_result.Syntax in
        let* () = Repo.Init.seed conn in
        let* found = Bibliography.ls'' () conn in

        Lwt.return_ok found
      in
      let ran = Lwt_main.run prom |> Result.map_error Caqti_error.show in
      ran
      => Ok
           [ (1, "OCaml from the Very Beginning")
           ; (2, "More OCaml")
           ; (3, "Programming in Haskell")
           ; (4, "Real World OCaml")
           ; (5, "Real World OCaml")
           ]
;;

let%test_unit "use seed, then read many rows, with ppx_rapper" =
  let ( => ) =
    [%test_eq:
      ( (Base.int
        * Base.string
        * Base.string
        * Base.string Base.option
        * Base.string)
        Base.list
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
        let* found = Bibliography.ls''' () conn in
        Lwt.return_ok found
      in
      let ran = Lwt_main.run prom |> Result.map_error Caqti_error.show in
      ran
      => Ok
           [ (1, "OCaml from the Very Beginning", "John", None, "Whitington")
           ; (2, "More OCaml", "John", None, "Whitington")
           ; (3, "Programming in Haskell", "Graham", None, "Hutton")
           ; (4, "Real World OCaml", "Anil", None, "Madhavapeddy")
           ; (5, "Real World OCaml", "Yaron", None, "Minsky")
           ]
;;

let%test_unit _ = [%test_eq: Base.int] 1 1

open Sexplib.Std

(* type ls4_row =
     { id : int
     ; title : string
     ; first_name : string
     ; middle_name : string option
     ; last_name : string
     }
   [@@deriving sexp, ord] *)

type my_rec = { id : int } [@@deriving sexp, ord]

let%test_unit _ = [%test_eq: my_rec] { id = 1 } { id = 1 }
(* let%test_unit _ = [%test_eq: ls4_row] { id = 1 } { id = 1 } *)

let%test_unit "use seed, then read many rows, with ppx_rapper, with records" =
  let ( => ) =
    [%test_eq: (Bibliography.ls4_row Base.list, Base.string) Base.Result.t]
  in
  let conn, setup = Setup.fresh_db () in
  match Lwt_main.run setup with
  | Error e -> Setup.fail e
  | Ok () ->
      let prom =
        let open Lwt_result.Syntax in
        let* () = Repo.Init.seed conn in
        let* found = Bibliography.ls4 () conn in
        Lwt.return_ok found
      in
      let ran = Lwt_main.run prom |> Result.map_error Caqti_error.show in
      ran
      => Ok
           [ { id = 1
             ; title = "OCaml from the Very Beginning"
             ; first_name = "John"
             ; middle_name = None
             ; last_name = "Whitington"
             }
           ; { id = 2
             ; title = "More OCaml"
             ; first_name = "John"
             ; middle_name = None
             ; last_name = "Whitington"
             }
           ; { id = 3
             ; title = "Programming in Haskell"
             ; first_name = "Graham"
             ; middle_name = None
             ; last_name = "Hutton"
             }
           ; { id = 4
             ; title = "Real World OCaml"
             ; first_name = "Anil"
             ; middle_name = None
             ; last_name = "Madhavapeddy"
             }
           ; { id = 5
             ; title = "Real World OCaml"
             ; first_name = "Yaron"
             ; middle_name = None
             ; last_name = "Minsky"
             }
           ]
;;
