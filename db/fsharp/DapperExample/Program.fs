open System.Data
open System.Threading.Tasks
open Dapper.FSharp.PostgreSQL
open Npgsql

[<Literal>]
let connStr =
    "Host=pg.mpg.test;Database=mpg_db;UserName=postgres;Password=postgres;"

(*
    Requires: Npgsql.dll => `Npgsql` package
*)
let conn: IDbConnection = new NpgsqlConnection(connStr)

(*
    I must set the type name = table name!
    I must set the field names = column names!
 *)
type Customer =
    { name: string
      alternative_name: string option }

let customerTable = table'<Customer> "customers"



let insertJohn altName : Task<int> =
    let newCustomer =
        { name = "John"
          alternative_name = altName }

    insert {
        into customerTable
        value newCustomer
    }
    |> conn.InsertAsync

let selectJohns () =
    select {
        for c in customerTable do
            where (ilike c.name "JOH%")
    }
    |> conn.SelectAsync<Customer>

let deleteJohns () =
    delete {
        for c in customerTable do
            where (ilike c.name "JOH%")
    }
    |> conn.DeleteAsync

[<EntryPoint>]
let main _ =
    (*
        Conclusion / observations
    
            - will compile with bad table/column names (will generate runtime errors)
            - SQL output looks clean for now
            - I don't think we can rename the table/column names, that's annoying
            - The query DSL looks quite flexible
                - I could not find a way to share their definitions (see delete vs select)
    *)
    OptionTypes.register ()

    Task.WaitAll(
        task {
            printfn "Deleting Johns..."
            let! n = deleteJohns ()
            printfn $"Deleted %d{n} rows"
            
            let! x = insertJohn None
            let! y = insertJohn (Some "Johnny")
            printfn $"Number of successful insertions: %d{x + y}"

            let! johns = selectJohns ()
            printfn $"Customers named John: %A{johns}"

            // for john in johns do
            // printfn $"- %A{john}"

            return ()
        }
    )

    0


