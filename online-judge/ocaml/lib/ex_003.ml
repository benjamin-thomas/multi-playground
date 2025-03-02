(*
   https://oj.moonbitlang.com/problems/202412-003-number-string-formatting

      dune exec ./test/test.exe test 003 -w
*)

let find_offset str =
  String.index_opt str '.'
  |> Option.map (fun pos -> String.length str - pos)
  |> Option.value ~default:0
;;

let remove_leading_zeros str =
  let len = String.length str in
  let (start_at, _) =
    String.fold_left
      begin
        fun (cursor, stop) c ->
          if stop || c <> '0' then
            (cursor, true)
          else
            (cursor + 1, false)
      end
      (0, false)
      str
  in
  String.sub str start_at (len - start_at)
;;

exception Break

let remove_leading_zeros2 str =
  let len = String.length str in
  let idx =
    let idx = ref 0 in
    try
      for i = 0 to len - 1 do
        if str.[i] <> '0' then
          raise Break
        else
          idx := i + 1
      done;
      !idx
    with
    | Break -> !idx
  in
  String.sub str idx (len - idx)
;;

let rev x =
  let len = String.length x in
  String.init len (fun n -> String.get x (len - n - 1))
;;

let insert_separator offset str =
  let chars =
    snd
    @@ String.fold_right
         begin
           fun c (n, acc) ->
             ( n + 1
             , c
               ::
               (if n > offset && n mod 3 = offset mod 3 then
                  ',' :: acc
                else
                  acc) )
         end
         str
         (0, [])
  in
  let buf = Buffer.create 1024 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf
;;

let solution str =
  let str2 = remove_leading_zeros str in
  if String.starts_with ~prefix:"." str2 then
    str
  else
    insert_separator (find_offset str2) str2
;;

let solution_bad str =
  let n = ref 0 in
  let buf = Buffer.create 1024 in
  for i = String.length str - 1 downto 0 do
    if !n > 0 && !n mod 3 = 0 then Buffer.add_char buf ',';
    Buffer.add_char buf str.[i];
    n := !n + 1
  done;
  rev @@ Buffer.contents buf
;;

let sub_solution str =
  let sep_pos =
    try String.index str '.' with
    | Not_found -> 0
  in
  let before = ref [] in
  let after = ref [] in
  for i = String.length str - 1 downto 0 do
    let c = str.[i] in
    if i >= sep_pos then
      before := c :: !before
    else
      after := c :: !after
  done;
  match !after with
  | [] -> solution (String.of_seq @@ List.to_seq !before)
  | _ ->
    solution (String.of_seq @@ List.to_seq !after) ^ String.of_seq @@ List.to_seq !before
;;
