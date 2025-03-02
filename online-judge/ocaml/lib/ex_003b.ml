let remove_leading_zeros str =
  let buf = Buffer.create (String.length str) in
  let stripping = ref true in
  String.iter
    (fun c ->
       if c = '0' && !stripping then
         ()
       else begin
         stripping := false;
         Buffer.add_char buf c
       end)
    str;
  Buffer.contents buf
;;

let solution str =
  let stripped = remove_leading_zeros str in
  let parts = String.split_on_char '.' stripped in
  let (int, dec) =
    match parts with
    | [ int ] -> (int, "")
    | [ int; dec ] -> (int, "." ^ dec)
    | _ -> ("", "")
  in
  let buf = Buffer.create (String.length int + String.length dec + 10) in
  let len = String.length int in
  for i = 0 to len - 1 do
    Buffer.add_char buf int.[i];
    if i < len - 1 && (len - i - 1) mod 3 = 0 then Buffer.add_char buf ','
  done;
  Buffer.add_string buf dec;
  Buffer.contents buf
;;
