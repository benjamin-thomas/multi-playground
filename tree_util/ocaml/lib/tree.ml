let add_trailing_sep_char str =
  let sepChar = Filename.dir_sep in
  if String.ends_with ~suffix:sepChar str then str else str ^ sepChar
;;

let enum_entries path =
  Sys.readdir path |> Array.to_list |> List.map (fun n -> add_trailing_sep_char path ^ n)
;;

type summary =
  { name : string
  ; is_dir : bool
  ; is_hidden : bool
  ; is_link : bool
  ; size : int
  ; children : summary list
  ; parent_dir : string
  }

let dir_name path = Filename.dirname path |> Filename.basename
let is_hidden path = path |> Filename.basename |> String.starts_with ~prefix:"."
let is_link path = (Unix.lstat path).st_kind = Unix.S_LNK

let rec to_summary path =
  let is_dir = Sys.is_directory path in
  let is_hidden = is_hidden path in
  let is_link = is_link path in
  let size = (Unix.stat path).st_size in
  let children =
    if is_dir && not is_link then enum_entries path |> List.concat_map to_summary else []
  in
  [ { name = path
    ; is_dir
    ; is_hidden
    ; is_link
    ; size
    ; children
    ; parent_dir = Filename.dirname path |> add_trailing_sep_char
    }
  ]
;;

let rec tree_to_string depth summary =
  summary
  |> List.filter (fun s -> not s.is_hidden) (* mimic os util: "tree" *)
  |> List.concat_map (fun summary ->
       let children = summary.children in
       if children <> []
       then
         [ depth, summary.size, summary.name, summary.is_dir, summary.parent_dir ]
         |> List.append (tree_to_string (depth + 1) children |> List.map Fun.id)
       else [ depth, summary.size, summary.name, summary.is_dir, summary.parent_dir ])
;;

(*

let hiddenFile summary = summary.IsHidden

let toLowerName summary = summary.Name.ToLower()
*)

let compute path = enum_entries path |> List.concat_map to_summary

(* TODO: make private fn *)
let to_human size = Printf.sprintf "%10s" (Int.to_string size)
let rec repeat n s = if n = 0 then "" else s ^ repeat (n - 1) s

(* TODO: make private fn *)
let print_path (depth, size, path, is_dir, parent_dir) : unit =
  let depth_to_ws = repeat depth "  " in
  (* let simple_path = Str.(global_replace (regexp parent_dir) "" path) in *)
  let simple_path = dir_name parent_dir ^ Filename.dir_sep ^ Filename.basename path in
  let file_type_emoji = if is_dir then "📁" else "📝" in
  (* let _ = Printf.printf "\n\n%s -> %s [%s]\n\n" path simple_path parent_dir in *)
  Printf.printf
    "[%s] %s [%d] %s %s\n"
    (to_human size)
    file_type_emoji
    depth
    depth_to_ws
    simple_path
;;

let print root_path =
  compute (root_path |> add_trailing_sep_char)
  |> tree_to_string 0
  |> List.sort (fun (_, _, path1, _, _) (_, _, path2, _, _) -> compare path1 path2)
  |> List.iter print_path
;;
