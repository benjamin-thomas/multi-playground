open Printf

module Char_list = struct
  let print lst =
    let body = String.concat "; " (List.map (sprintf "'%c'") lst) in
    print_string @@ "[" ^ body ^ "]"
  ;;
end

module String_list = struct
  let print lst =
    let items = List.map (sprintf {|"%s"|}) lst in
    print_string @@ "[" ^ String.concat "; " items ^ "]"
  ;;
end
