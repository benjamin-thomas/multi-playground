open FSharp.Data.Npgsql // https://github.com/demetrixbio/FSharp.Data.Npgsql

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
        | Some alt_name -> printfn $"Customer #%04d{x.id} -> %s{x.name} (%s{alt_name})"
        | None -> printfn $"Customer #%04d{x.id} -> %s{x.name}"

let test_list_tuples =
    use cmd =
        Db.CreateCommand<"SELECT id, name FROM customers", ResultType.Tuples>(connStr)

    let res: (int * string) list = cmd.Execute()
    res |> printfn "Customers as tuples: %A"

type Customer =
    { Id: int
      Name: string
      AltName: string option }

let getCustomersCmd =
    Db.CreateCommand<"SELECT id, name, alternative_name FROM customers WHERE id < @maxId LIMIT @limit", ResultType.Records>

// The properties must be specified sorted alphabetically
type GeneratedFromGetCustomerCmdExec = Db.``alternative_name:Option<String>, id:Int32, name:String``

let mapFromGetCustomersCmd (x: GeneratedFromGetCustomerCmdExec) : Customer =
    { Id = x.id
      Name = x.name
      AltName = None }

let test_list_records =
    use cmd = getCustomersCmd (connStr)

    // Using named params! It removes params order dependency.
    let res = cmd.Execute(limit = 3, maxId = 10)

    res |> printfn "Customers as records: %A"

    res |> List.map mapFromGetCustomersCmd |> printfn "Mapped customers: %A"

let test_list_anon_records =
    use cmd =
        Db.CreateCommand<"SELECT id, name, alternative_name FROM customers WHERE id < @maxId LIMIT @limit", ResultType.Records>(
            connStr
        )

    let res: List<Db.``alternative_name:Option<String>, id:Int32, name:String``> =
        // Use named params!
        cmd.Execute(limit = 3, maxId = 10)

    res
    |> List.map (fun c ->
        {| Id = c.id
           Name = c.name
           AlternativeName = c.alternative_name |})


[<EntryPoint>]
let main (_) =
    printfn "=== Testing database interactivity ==="

    list_customers_freestyle
    test_list_tuples
    test_list_records

    printfn "=> Using anonymous records..."

    test_list_anon_records
    |> List.iter (fun customer ->
        printfn $"ID      = %03d{customer.Id}\nName    = %s{customer.Name}\nAltName = %A{customer.AlternativeName}\n")

    0
