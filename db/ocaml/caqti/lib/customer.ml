(*
  dune utop lib
  > Lib.Customer.greet;;
*)

open Caqti_request
open Caqti_request.Infix

type customer =
  { id : int
  ; name : string
  ; alternative_name : string option
  }

type error = Database_error of string

let connection_url = "postgresql://postgres:postgres@pg.mpg.test:5432/mpg_db"

let pool =
  match Caqti_lwt.connect_pool ~max_size:2 (Uri.of_string connection_url) with
  | Ok pool -> pool
  | Error err -> failwith (Caqti_error.show err)
;;

module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let customer =
    let encode { id; name; alternative_name } = Ok (id, name, alternative_name) in
    let decode (id, name, alternative_name) = Ok { id; name; alternative_name } in
    let rep = Caqti_type.(tup3 int string (option string)) in
    custom ~encode ~decode rep
  ;;

  let add =
    (tup2 string (option string) ->. unit)
    @@ "INSERT INTO customers (name, alternative_name) VALUES (?, ?)"
  ;;

  let change_name =
    (tup2 string int ->. unit) @@ "UPDATE customers SET name = ? WHERE id = ?"
  ;;

  let list = (unit ->* customer) @@ "SELECT * FROM customers WHERE id < 10"
end

(* Helper method to map Caqti errors to our own error type.
   val or_error : ('a, [> Caqti_error.t ]) result Lwt.t -> ('a, error) result Lwt.t *)
let or_error m =
  match%lwt m with
  | Ok a -> Ok a |> Lwt.return
  | Error e -> Error (Database_error (Caqti_error.show e)) |> Lwt.return
;;

let get_all_query =
  Caqti_request.collect
    Caqti_type.unit
    Caqti_type.(tup3 int string (option string))
    "SELECT id, name, alternative_name FROM customers"
;;

let get_all () =
  let get_all' (module C : Caqti_lwt.CONNECTION) =
    C.fold
      get_all_query
      (fun (id, name, alternative_name) acc -> { id; name; alternative_name } :: acc)
      ()
      []
  in
  Caqti_lwt.Pool.use get_all' pool |> or_error
;;

let printCustomer customer = print_endline customer.name
let printCustomers (customers : customer list) = customers |> List.iter printCustomer

let fetchAll () =
  print_endline "=> Fetching customers...";
  match%lwt get_all () with
  | Ok customers -> printCustomers customers |> Lwt.return
  | Error (Database_error msg) -> print_endline msg |> Lwt.return
;;

(* Db.iter_s iterates sequentially over the set of result rows of a query. *)
let iter_s_list (module Db : Caqti_lwt.CONNECTION) f = Db.iter_s Q.list f ()
let greet = "hello"
