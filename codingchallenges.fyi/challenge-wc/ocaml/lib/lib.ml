module Bytes = struct
  let count_line line = String.fold_left (fun acc _ -> acc + 1) 1 line
end

module Words = struct
  let is_white_space = function
    | ' ' | '\r' | '\n' | '\t' -> true
    | _ -> false
  ;;

  let count_line line =
    let update_count (count, in_word) ch =
      if is_white_space ch then
        (count, false)
      else if not in_word then
        (count + 1, true)
      else
        (count, true)
    in
    fst (String.fold_left update_count (0, false) line)
  ;;
end

module Runes = struct
  let count_line line =
    let add acc _ = function
      | `Uchar _ -> acc + 1
      | `Malformed _bs -> failwith "Bad encoding (probably)"
    in
    Uutf.String.fold_utf_8 add 1 line
  ;;
end
