(* ─── ppx_minidebug 3.x runtime selector ─────────────────────────────────────
   Flip [backend] below to switch where debug logs go. The ppx looks up the magic
   name [_get_local_debug_runtime] (leading underscore => referenced only by
   generated code, so no "unused" warning).

   Stdout : pretty tree printed straight to the terminal (time-tagged here).
              dune exec bin/wip3.exe

   Sqlite : writes wip3.db, browsed with the interactive viewer.
              dune exec bin/wip3.exe
              dune exec minidebug_view -- wip3.db tui     (interactive TUI)
              dune exec minidebug_view -- wip3.db show    (print the tree)
   ──────────────────────────────────────────────────────────────────────────── *)

type debug_backend = Stdout | Sqlite [@@warning "-37"]

(* ← change this one value *)
let backend = Stdout

let _get_local_debug_runtime =
  match backend with
  | Stdout -> Minidebug_runtime.prefixed_runtime ~time_tagged:Clock ()
  | Sqlite ->
      let rt = Minidebug_db.debug_db_file ~time_tagged:Clock "wip3" in
      fun () -> rt
;;

let%debug_show test_logging : string = "Hello World"
let () = print_endline test_logging
