(*
   dune exec --no-print-directory ./main.exe < ../../_inputs/day01.txt
*)

module Curr = Aoc.Day01
open Printf

let () =
  let input = In_channel.input_all In_channel.stdin in
  let lines = String.split_on_char '\n' input in
  ()
  ; printf "Part 1: %d\n" @@ Curr.part1 lines
  ; printf "Part 2: %d\n" @@ Curr.part2 lines
;;
