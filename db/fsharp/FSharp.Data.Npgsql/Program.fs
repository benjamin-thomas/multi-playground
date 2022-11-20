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
        | Some alt_name -> printfn "Customer #%04d -> %s (%s)" x.id x.name alt_name
        | None -> printfn "Customer #%04d -> %s" x.id x.name

let test_list_tuples =
    use cmd =
        Db.CreateCommand<"SELECT id, name FROM customers", ResultType.Tuples>(connStr)

    let res: (int * string) list = cmd.Execute()
    res |> printfn "Customers as tuples: %A"

type Customer =
    {
        Id: int
        Name: string
        AlternativeName: string option
    }

let test_list_records =
    use cmd =
        Db.CreateCommand<"SELECT id, name FROM customers WHERE id < @maxId LIMIT @limit", ResultType.Records>(connStr)

    let res: List<Db.``id:Int32, name:String``> =
        // Use named params!
        cmd.Execute(limit = 3, maxId = 10)

    res |> printfn "Customers as records: %A"

    // Code load order issue: this generated type is not available before `cmd.Execute()`
    // So I can only define this function here for now.
    let mapCustomer (x: Db.``id:Int32, name:String``) : Customer =
        {
            Id = x.id
            Name = x.name
            AlternativeName = None
        }

    res
    |> List.map mapCustomer
    |> printfn "Mapped customers: %A"

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
        {|
            Id = c.id
            Name = c.name
            AlternativeName = c.alternative_name
        |})


[<EntryPoint>]
let main (_) =
    printfn "=== Testing database interactivity ==="

    list_customers_freestyle
    test_list_tuples
    test_list_records

    printfn "=> Using anonymous records..."

    test_list_anon_records
    |> List.iter (fun cust ->
        printfn "ID      = %03d\nName    = %s\nAltName = %A\n" cust.Id cust.Name cust.AlternativeName)

    0
