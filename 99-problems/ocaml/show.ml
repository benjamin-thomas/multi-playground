open Printf

let tup2 (a, b) = "(" ^ a ^ ", " ^ b ^ ")"

let char_list lst =
  let body = String.concat "; " (List.map (sprintf "'%c'") lst) in
  "[" ^ body ^ "]"
;;

let string_list lst =
  let items = List.map (sprintf {|"%s"|}) lst in
  let body = String.concat "; " items in
  "[" ^ body ^ "]"
;;

let int_list lst =
  let items = List.map string_of_int lst in
  let body = String.concat "; " items in
  "[" ^ body ^ "]"
;;
