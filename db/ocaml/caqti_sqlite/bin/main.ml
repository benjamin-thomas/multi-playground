(*
 * Creates and seeds an Sqlite database, then prints some of its content.
 *
 * Usage:
 *   rm ./db.sqlite3;dune exec ./bin/main.exe
 *)

module Init = Repo.Init
module Bibliography = Repo.Bibliography

(*
 * UTILS
 *)
let info_log fmt = Printf.printf ("[INFO] " ^^ fmt ^^ "\n%!")
let err_log fmt = Printf.printf ("[ERROR] " ^^ fmt ^^ "\n%!")

let longest_book_name bibliography =
  List.fold_left
    (fun longest (_id, book_name, _author_fname, _author_lname) ->
      max longest (String.length book_name))
    0 bibliography
;;

let print_item padding (_id, book_name, author_fname, author_lname) =
  Printf.printf "%-*s: %s %s\n" padding book_name author_fname author_lname
;;

(*
 * BOOTSTRAP
 *)
let () =
  let open Lwt_result.Syntax in
  let conn = Init.caqti_conn () in
  let work : ('bibliography, 'error) result Lwt.t =
    let* () = Init.create_tables conn in
    let* () = Init.seed conn in
    let* found = Bibliography.ls conn () in
    Lwt.return_ok found
  in

  Lwt_main.run work |> function
  | Ok bibliography ->
      let longest = longest_book_name bibliography in
      ()
      ; info_log "Setup OK!"
      ; print_newline ()
      ; print_endline "Bibliography"
      ; print_endline "============"
      ; print_newline ()
      ; List.iter (print_item (longest + 1)) bibliography
  | Error e -> err_log "%s" (Caqti_error.show e)
;;
