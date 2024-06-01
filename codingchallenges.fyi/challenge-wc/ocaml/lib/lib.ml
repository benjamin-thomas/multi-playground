[@@@warning "-32"]

let count_bytes path =
  let count = ref 0 in
  In_channel.with_open_bin path
  @@ fun ic ->
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

let count_bytes path =
  let rec aux n ic =
    try
      ()
      ; input_char ic |> ignore
      ; aux (n + 1) ic
    with
    | End_of_file -> n
  in
  In_channel.with_open_bin path (aux 0)
;;

let count_lines path =
  let rec aux n ic =
    try
      ()
      ; input_line ic |> ignore
      ; aux (n + 1) ic
    with
    | End_of_file -> n
  in
  In_channel.with_open_bin path (aux 0)
;;
