(* ─── ppx_minidebug 3.x runtime selector ─────────────────────────────────────
   Flip [backend] below to switch where debug logs go. The ppx looks up the magic
   name [_get_local_debug_runtime] (leading underscore => referenced only by
   generated code, so no "unused" warning).

   Stdout : pretty tree printed straight to the terminal.
              dune exec bin/wip2.exe

   Sqlite : writes wip2.db, browsed with the interactive viewer.
              dune exec bin/wip2.exe
              dune exec minidebug_view -- wip2.db tui     (interactive TUI)
              dune exec minidebug_view -- wip2.db show    (print the tree)
   ──────────────────────────────────────────────────────────────────────────── *)

type debug_backend = Stdout | Sqlite [@@warning "-37"]

(* ← change this one value *)
let backend = Stdout

let _get_local_debug_runtime =
  match backend with
  | Stdout -> Minidebug_runtime.prefixed_runtime ()
  | Sqlite ->
      let rt = Minidebug_db.debug_db_file "wip2" in
      fun () -> rt
;;

(*
let%debug_show rec sub_sets (input : char list) : char list list =
  match input with
  | [] -> [ [] ]
  | h :: t ->
      let left : char list list = sub_sets t in
      let right : char list list = List.map (List.cons h) left in
      left @ right
;; *)

let%debug_show sets_of n (lst : char list) =
  let rec sub_sets (lst : char list) (n : int) : char list list =
    match lst with
    | [] -> [ [] ]
    | h :: t ->
        let left = sub_sets t (n - 1) in
        let right =
          if n > 0 then
            List.map (List.cons h) left
          else
            []
        in
        left @ right
  in
  sub_sets lst n
;;

let () =
  let _ = sets_of 2 [ 'A'; 'B'; 'C' ] in
  print_endline "Done!"
;;
