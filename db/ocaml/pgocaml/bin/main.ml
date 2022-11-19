(*
  export PGHOST=pg.mpg.test PGDATABASE=mpg_db PGUSER=postgres PGPASSWORD=postgres
  rg --files | entr -c dune exec --display=quiet pgocaml_demo
*)

(*
PPX is broken for PG version >= 14.0
  https://github.com/darioteixeira/pgocaml/issues/120
*)
let () =
  let dbh : 'a PGOCaml.t = PGOCaml.connect () in
  let getCustomer name =
    [%pgsql dbh "SELECT alternative_name FROM customers WHERE name = $name"]
  in
  let name = "John" in
  let altName =
    getCustomer name
    |> List.hd
    |> function
    | Some x -> x
    | None -> raise (Failure "The database is probably broken.")
  in
  Printf.printf "%s's alternative name is: %s\n" name altName;
  print_endline "=> Closing DB connection...";
  PGOCaml.close dbh;
  print_endline @@ "=> " ^ Lib.Messages.finished
;;
