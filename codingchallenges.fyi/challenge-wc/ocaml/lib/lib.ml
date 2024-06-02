module Bytes = struct
  let[@warning "-32"] count ic =
    let count = ref 0 in
    let () =
      try
        while true do
          let _ = input_char ic in
          count := !count + 1
        done
      with
      | End_of_file -> ()
    in
    !count
  ;;

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
  let[@warning "-32"] count ic =
    let rec aux n =
      try
        ()
        ; input_line ic |> ignore
        ; aux (n + 1)
      with
      | End_of_file -> n
    in
    aux 0
  ;;

  let[@warning "-32"] count ic =
    let rec aux n =
      match In_channel.input_line ic with
      | None -> n
      | Some _ -> aux (n + 1)
    in
    aux 0
  ;;

  (* Just for fun: I prefer the version above *)
  let count ic =
    let rec aux n =
      In_channel.input_line ic
      |> Option.map (fun _ -> aux (n + 1))
      |> Option.value ~default:n
    in
    aux 0
  ;;
end

let is_white_space = function
  | ' ' | '\r' | '\n' | '\t' -> true
  | _ -> false
;;

module Words = struct
  let[@warning "-32"] count_line_words line =
    let len = String.length line in
    let word_count = ref 0 in
    let in_word = ref false in
    let () =
      for i = 0 to len - 1 do
        let curr = line.[i] in
        if is_white_space curr then
          in_word := false
        else if not !in_word then (
          ()
          ; in_word := true
          ; incr word_count
        )
      done
    in
    !word_count
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

  let[@warning "-32"] count ic =
    let rec aux n =
      try
        let line = input_line ic in
        let words = count_line_words line in
        aux (n + words)
      with
      | End_of_file -> n
    in
    aux 0
  ;;

  let count ic =
    let rec aux n =
      match In_channel.input_line ic with
      | None -> n
      | Some line -> aux (n + count_line_words line)
    in
    aux 0
  ;;

  (* TESTING *)
  let%test_module "update_count" =
    (module struct
      let update_count (count, in_word) ch =
        if is_white_space ch then
          (count, false)
        else if in_word then
          (count, true)
        else
          (count + 1, true)
      ;;

      open Base

      (* LEFT: the current count, RIGHT: whether we are in a word *)

      let%test_unit "whitespace, no change if already in word" =
        [%test_eq: int * bool] (update_count (0, false) ' ') (0, false)
      ;;

      let%test_unit "whitespace, transition out of word" =
        [%test_eq: int * bool] (update_count (1, true) ' ') (1, false)
      ;;

      let%test_unit "non-whitespace, no change if already in word" =
        [%test_eq: int * bool] (update_count (1, true) 'B') (1, true)
      ;;

      let%test_unit "non-whitespace, increase count + transition to in_word if not in \
                     word"
        =
        [%test_eq: int * bool] (update_count (0, false) 'A') (1, true)
      ;;
    end)
  ;;

  let%test_unit _ =
    ()
    ; [%test_eq: Base.int] 0 (count_line_words @@ "")
    ; [%test_eq: Base.int] 0 (count_line_words @@ "   ")
    ; [%test_eq: Base.int] 1 (count_line_words @@ "x")
    ; [%test_eq: Base.int] 1 (count_line_words @@ "xy")
    ; [%test_eq: Base.int] 0 (count_line_words @@ " ")
    ; [%test_eq: Base.int] 1 (count_line_words @@ " x")
    ; [%test_eq: Base.int] 0 (count_line_words @@ "  ")
    ; [%test_eq: Base.int] 1 (count_line_words @@ "  x")
    ; [%test_eq: Base.int] 1 (count_line_words @@ "  xy")
    ; [%test_eq: Base.int] 1 (count_line_words @@ "  xyz")
    ; [%test_eq: Base.int] 1 (count_line_words @@ "  xyz ")
    ; [%test_eq: Base.int] 2 (count_line_words @@ "  xyz a")
    ; [%test_eq: Base.int] 3 (count_line_words @@ "  xyz a b")
    ; [%test_eq: Base.int] 3 (count_line_words @@ "xyz a b")
    ; [%test_eq: Base.int] 2 (count_line_words @@ "x y")
    ; [%test_eq: Base.int] 3 (count_line_words @@ "x y z")
    ; [%test_eq: Base.int] 3 (count_line_words @@ "  x y z")
    ; [%test_eq: Base.int] 3 (count_line_words @@ " x   yyy    z ")
  ;;
end

(* RUNES *)

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
