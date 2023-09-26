open Printf

module Tuple = struct
  let show (a, b) = "(" ^ a ^ ", " ^ b ^ ")"
end

module Char_list = struct
  let show lst =
    let body = String.concat "; " (List.map (sprintf "'%c'") lst) in
    "[" ^ body ^ "]"
  ;;
end

module String_list = struct
  let show lst =
    let items = List.map (sprintf {|"%s"|}) lst in
    "[" ^ String.concat "; " items ^ "]"
  ;;
end
