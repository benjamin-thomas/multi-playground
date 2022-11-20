﻿open FSharp.Data.Npgsql // https://github.com/demetrixbio/FSharp.Data.Npgsql

(*
    Initialize the database with:
        ../../init.ml

    Build release fails if column has been renamed (good!)
        dotnet build -c Release

    PGUSER/PGPASSWORD env vars can sourced instead of providing the UserName/Password params below.
*)
[<Literal>]
let connStr =
    "Host=pg.mpg.test;Database=mpg_db;UserName=postgres;Password=postgres;"

type Db = NpgsqlConnection<connStr, ReuseProvidedTypes=true>


let list_customers_freestyle =
    use cmd =
        Db.CreateCommand<"SELECT id, name, alternative_name AS alt_name FROM customers">(connStr)

    for (x) in cmd.Execute() do

        match x.alt_name with
        | Some alt_name -> printfn "Customer #%04d -> %s (%s)" x.id x.name alt_name
        | None -> printfn "Customer #%04d -> %s" x.id x.name

let test_list_tuples =
    use cmd =
        Db.CreateCommand<"SELECT id, name FROM customers", ResultType.Tuples>(connStr)

    let res: (int * string) list = cmd.Execute()
    res |> printfn "Customers as tuples: %A"

let test_list_records =
    use cmd =
        Db.CreateCommand<"SELECT id, name FROM customers", ResultType.Records>(connStr)

    let tmp = {| Id = 1; Name = "Ben" |}
    // let res: List<Db.``id:Int32, name:String``> = cmd.Execute()
    let res: (Db.``id:Int32, name:String``) list = cmd.Execute()
    res |> printfn "Customers as records: %A"


(*
-------------------------------------------
FIXME: I could not get this code to compile
-------------------------------------------
type Customer =
    {
        Id: int
        Name: string
        AlternativeName: string option
    }

// CreateCommand returning a type we want to refer to in a function signature has to be 'mentioned' first
let getAllCustomersCommand =
    Db.CreateCommand<"SELECT id, name, alternative_name FROM customers">

// The type with title and rating is now generated and accessible
// error FS0039: The type 'id:String, name:String, alternative_name:Option<String>' is not defined.
let mapCustomer (x: Db.``id:String, name:String, alternative_name:Option<String>``) : Customer =
    {
        Id = x.id
        Name = x.name
        AlternativeName = x.alternative_name
    }

let getAllCustomers () =
    use cmd = getAllCustomersCommand connStr
    let res = cmd.Execute()
    res |> List.map mapCustomer
*)

[<EntryPoint>]
let main (_) =
    printfn "=== Testing database interactivity ==="

    list_customers_freestyle
    test_list_tuples
    test_list_records

    0
