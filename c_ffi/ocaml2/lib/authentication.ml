(* https://dune.readthedocs.io/en/stable/foreign-code.html#a-toy-example *)

let verify_password hash password =
  let password_len = String.length password in
  let result =
    C.Functions.argon2id_verify hash password (Unsigned.Size_t.of_int password_len)
  in
  0 = result
;;

let hash = "$argon2id$v=19$m=16,t=2,p=1$c2FsdC02Nzg$XUb4CH1RppZ2CLAi36TUrw"
let verify password = verify_password hash password
