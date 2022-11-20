(*
  export PGHOST=pg.mpg.test PGDATABASE=mpg_db PGUSER=postgres PGPASSWORD=postgres
  rg --files | entr -c dune exec --display=quiet pgocaml_demo

  dune build --profile release
  _build/default/bin/main.exe
*)

(*
PPX is broken for PG version >= 14.0
  https://github.com/darioteixeira/pgocaml/issues/120
*)

(* let customer_to_string ((id : int32), (name : string), Some alt_name) =
  String.trim
  @@ Printf.sprintf {|
     ID = %ld
   Name = %s
AltName = %s
|} id name alt_name
;; *)

let new_customers_data =
  (* TODO: transpose horizontal layout -> vertical layout *)
      (* Sue↓ *)     (* Bob↓ *)
  ( [ Some "Sue";    Some "Robert" ] (* names *)
  , [ None;          Some "Bob" ]    (* alt names *)
  )
  [@@ocamlformat "disable"]

let customer_to_string ((id : int32), (name : string), (alt_name : string option)) =
  match alt_name with
  | None -> Printf.sprintf "ID      = %ld\nName    = %s\n" id name
  | Some alt_name ->
    Printf.sprintf "ID      = %ld\nName    = %s\nAltName = %s\n" id name alt_name
;;

let ( << ) f g x = f (g x)

let () =
  let db : 'a PGOCaml.t = PGOCaml.connect () in
  (*
   * QUERIES
   *)
  let get_customer_by_name name =
    [%pgsql.object
      db "SELECT id, name, alternative_name FROM customers WHERE name = $name"]
  in
  let list_customers ~limit =
    [%pgsql db "SELECT id, name, alternative_name FROM customers LIMIT $limit"]
  in
  let insert_customers (names, alt_names) =
    [%pgsql
      db
        "INSERT INTO customers (name, alternative_name) SELECT * FROM \
         UNNEST($names::text[], $alt_names::text[])"]
  in
  let delete_by_any_name name =
    [%pgsql db "DELETE FROM customers WHERE name = $name OR alternative_name = $name"]
  in
  (*
   * CRUD
   *)
  let john =
    let name_param = "John" in
    get_customer_by_name name_param
    |> function
    | [] -> failwith @@ Printf.sprintf "Customer not found (by name '%s')" name_param
    | x :: _ -> x
    (* |> function
    | Some x -> x
    | None -> failwith @@ Printf.sprintf "Customer not found (by name '%s')" name_param *)
  in
  (*
   * READ
   *)
  print_endline "=> Showing current customers...";
  list_customers ~limit:99L |> List.iter (print_endline << customer_to_string);
  print_endline "=> Searching for John...";
  Printf.printf "John's ID is: %ld\n\n" john#id;
  (*
   * CREATE
   *)
  print_endline "=> Inserting Sue and Bob...\n";
  insert_customers new_customers_data;
  print_endline "=> Showing current customers, again...";
  list_customers ~limit:99L |> List.iter (print_endline << customer_to_string);
  (*
   * DELETE
   *)
  print_endline "=> Deleting Sue(s) and Robert(s)...\n";
  (* delete by name *)
  delete_by_any_name "Sue";
  (* delete by alt name *)
  delete_by_any_name "Bob";
  (*
   * FINISHED
   *)
  print_endline "=> Showing current customers, again...";
  list_customers ~limit:99L |> List.iter (print_endline << customer_to_string);
  print_endline "=> Closing DB connection...";
  PGOCaml.close db;
  print_endline @@ "=> " ^ Lib.Messages.finished
;;
