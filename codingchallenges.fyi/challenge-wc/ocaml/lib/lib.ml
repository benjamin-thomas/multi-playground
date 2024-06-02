module Bytes = struct
  let count ic =
    let rec aux n =
      match In_channel.input_char ic with
      | None -> n
      | Some _ -> aux (n + 1)
    in
    aux 0
  ;;
end

module Lines = struct
  let count ic =
    let rec aux n =
      match In_channel.input_line ic with
      | None -> n
      | Some _ -> aux (n + 1)
    in
    aux 0
  ;;
end

module Words = struct
  let is_white_space = function
    | ' ' | '\r' | '\n' | '\t' -> true
    | _ -> false
  ;;

  let count_line_words line =
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

  let count ic =
    let rec aux n =
      match In_channel.input_line ic with
      | None -> n
      | Some line -> aux (n + count_line_words line)
    in
    aux 0
  ;;
end

module Runes = struct
  let count_line_runes line =
    let add acc _ = function
      | `Uchar _ -> acc + 1
      | `Malformed _bs -> failwith "Bad encoding (probably)"
    in
    Uutf.String.fold_utf_8 add 1 line
  ;;

  let count ic =
    let rec aux n =
      match In_channel.input_line ic with
      | None -> n
      | Some line ->
        let count = count_line_runes line in
        aux (n + count)
    in
    aux 0
  ;;
end
