let () =
  let args = Sys.argv in
  if Array.length args <> 2
  then (
    Printf.printf "Usage: %s <password>\n%!" args.(0);
    exit 1)
  else (
    let password = args.(1) in
    if Authentication.verify password
    then Printf.printf "Password is correct!\n%!"
    else Printf.printf "Password is incorrect!\n%!")
;;
