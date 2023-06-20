(*
   dune exec ./main.exe
   dune exec ./main.exe -w
 *)

let dbName = "mpg_db"
let createDb = Printf.sprintf "CREATE DATABASE %s;" dbName
let dropDb = Printf.sprintf "DROP DATABASE %s;" dbName

let createTables =
  {|
CREATE TABLE customers (
    id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , name VARCHAR(50) NOT NULL
  , alternative_name VARCHAR(60) NULL
);
|}
  |> String.trim
;;

let seedTables =
  {|
INSERT INTO customers (name, alternative_name) VALUES
    ('John', 'Johnny')
  , ('Mary', NULL)
;
|}
  |> String.trim
;;

let log msg =
  let tm = Unix.time () |> Unix.localtime in
  Printf.printf
    "[%04d-%d-%d %02d:%02d:%02d] => %s\n"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec
    msg
;;

let fail_on_err n = if n = 0 then () else failwith (Printf.sprintf "Exit code: %d" n)

let psqlRoot cmd =
  let relPath = Filename.dirname Sys.argv.(0) ^ "/../../../psql_root" in
  Filename.quote_command relPath [ "-qc"; cmd ]
;;

let psql cmd =
  let relPath = Filename.dirname Sys.argv.(0) ^ "/../../../psql" in
  Filename.quote_command relPath [ "-qc"; cmd ]
;;

let _ =
  log "Reset database!";
  Sys.command (psqlRoot dropDb) |> ignore;
  Sys.command (psqlRoot createDb) |> ignore;
  log "Create tables";
  Sys.command (psql createTables) |> fail_on_err;
  log "Seed tables";
  Sys.command (psql seedTables) |> fail_on_err;
  log "Done!"
;;
