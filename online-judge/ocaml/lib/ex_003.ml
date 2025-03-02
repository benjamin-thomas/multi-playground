(*
   https://oj.moonbitlang.com/problems/202412-003-number-string-formatting

      dune exec ./test/test.exe test 003 -w
*)

let rev x =
  let len = String.length x in
  String.init len (fun n -> String.get x (len - n - 1))
;;

let solution str =
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
