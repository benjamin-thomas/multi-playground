(*
To update the generated code, use:

dotnet npgsql-generator generate all -c "Host=pg.mpg.test;Database=mpg_db;UserName=postgres;Password=postgres;" ./sql/Customer.sql
*)

open System.Threading.Tasks
open Generated.Generated
open Npgsql

let connStr =
    "Host=pg.mpg.test;Database=mpg_db;UserName=postgres;Password=postgres;"

(*
    Requires: Npgsql.dll => `Npgsql` package
*)
let work (repo: ICustomerRepository) conn =
    task {

        let! johns = repo.GetCustomerByName conn "John"

        for c in johns do
            printfn $"Got result: %A{c}"

        printfn "Hello from F#2"
    }

let () =
    let conn: NpgsqlConnection = new NpgsqlConnection(connStr)
    let customerRepo = CustomerRepository.create ()
    Task.WaitAll(
        work customerRepo conn
    )
