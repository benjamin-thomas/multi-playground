open Printf

type instruction =
  | Mul of int * int
  | TurnOn
  | TurnOff

let[@warning "-32"] string_of_instr = function
  | Mul (a, b) -> sprintf "[MUL: %d * %d]" a b
  | TurnOn -> "[TURN ON]"
  | TurnOff -> "[TURN OFF]"
;;

(*
   Similarly to the Ruby regex library:

   Str.matched_group 0: returns the whole match
   Str.matched_group 1: returns the first captured group
   etc.

   OCaml's internal regex library doesn't seem to support the following:

   - named captured groups
   - `\d` for digits
*)

let example1 = {|xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))|}

let example2 =
  {|xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))|}
;;

module Part1 : sig
  val eval : (int * int) list -> int
  val make_instructions : int -> string -> (int * int) list
  val next : int -> string -> (int * (int * int)) option
end = struct
  let re = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|}

  let next offset str =
    try
      ignore @@ Str.search_forward re str offset;
      let a = int_of_string @@ Str.matched_group 1 str in
      let b = int_of_string @@ Str.matched_group 2 str in
      Some (Str.match_end () - 1, (a, b))
    with
    | Not_found -> None
  ;;

  let rec make_instructions offset str =
    match next offset str with
    | None -> []
    | Some (new_pos, instr) -> instr :: make_instructions new_pos str
  ;;

  let eval = List.fold_left (fun total (a, b) -> total + (a * b)) 0
end

module Part2 : sig
  val eval : instruction list -> int
  val make_instructions : int -> string -> instruction list
end = struct
  let re = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))\|do()\|don't()|}

  let next offset str =
    try
      ignore @@ Str.search_forward re str offset;
      let found = Str.matched_group 0 str in
      match found with
      | "don't()" -> Some (Str.match_end () - 1, TurnOff)
      | "do()" -> Some (Str.match_end () - 1, TurnOn)
      | _ ->
        Part1.next offset str
        |> Option.map @@ fun (new_pos, (a, b)) -> (new_pos, Mul (a, b))
    with
    | Not_found -> None
  ;;

  let rec make_instructions offset str =
    match next offset str with
    | None -> []
    | Some (new_pos, instr) -> instr :: make_instructions new_pos str
  ;;

  let eval instructions =
    fst
    @@ List.fold_left
         (fun (total, is_on) instr ->
           begin
             match instr with
             | TurnOn -> (total, true)
             | TurnOff -> (total, false)
             | Mul (a, b) -> ((total + if is_on then a * b else 0), is_on)
           end)
         (0, true)
         instructions
  ;;
end

let solve1 str = Part1.(eval @@ make_instructions 0 str)
let solve2 str = Part2.(eval @@ make_instructions 0 str)

let () =
  printf "Example 1: %d\n" (solve1 example1);
  (* Part2.make_instructions 0 example2 |> List.iter (fun instr -> printf ">>> %s\n" (string_of_instr instr)); *)
  printf "Example 2: %d\n" (solve2 example2);
  (* Solutions *)
  In_channel.with_open_text "../_inputs/03.txt"
  @@ fun ic ->
  let str = In_channel.input_all ic in
  printf "Part 1: %d\n" (solve1 str) (* Want: 178794710 *);
  printf "Part 2: %d\n" (solve2 str) (* Want: 76729637 *);
  ()
;;
