module Argon2 = struct
  open Ctypes

  let foreign = Foreign.foreign
  let libargon2 = Dl.dlopen ~filename:"libargon2.so" ~flags:[ Dl.RTLD_LAZY ]

  let verify =
    foreign
      "argon2id_verify"
      (string @-> string @-> size_t @-> returning int)
      ~from:libargon2
  ;;
end

let verify_password hash password =
  let password_len = String.length password in
  let result = Argon2.verify hash password (Unsigned.Size_t.of_int password_len) in
  0 = result
;;

let hash = "$argon2id$v=19$m=16,t=2,p=1$c2FsdC02Nzg$XUb4CH1RppZ2CLAi36TUrw"

let () =
  let args = Sys.argv in
  if Array.length args <> 2
  then (
    Printf.printf "Usage: %s <password>\n%!" args.(0);
    exit 1)
  else (
    let password = args.(1) in
    if verify_password hash password
    then Printf.printf "Password is correct!\n%!"
    else Printf.printf "Password is incorrect!\n%!")
;;
