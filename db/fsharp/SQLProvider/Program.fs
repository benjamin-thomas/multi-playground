open FSharp.Data.Sql

(*
    Initialize the database with:
        ../../init.ml

    Build release did NOT fail when I renamed a column has been renamed (bad!)
    Build release did fail when I restarted my IDE (??)
    I suppose there must be a build step that only the IDE initiates for now

    PGUSER/PGPASSWORD env vars can sourced instead of providing the UserName/Password params below.
*)


[<Literal>]
let connStr =
    "Host=pg.mpg.test;Database=mpg_db;UserName=postgres;Password=postgres;"

(*
    Requires: Npgsql.dll => `Npgsql` package
*)
type Db =
    SqlDataProvider<DatabaseVendor=Common.DatabaseProviderTypes.POSTGRESQL, ConnectionString=connStr, UseOptionTypes=Common.NullableColumnType.OPTION>

let ctx = Db.GetDataContext()
let customers = ctx.Public.Customers

type Customer =
    {
        Id: int
        Name: string
        AltName: string option
    }

let listCustomers () =
    query {
        for c in ctx.Public.Customers do
            where (c.Id > 0) // bogus, just for demo
            sortBy (c.Id) // how to sort by nulls first/last?
            // sortBy("name") // can't do that
            // sortByNullable (System.Nullable c.Id) // not sure why that'd be useful

            select (
                {
                    Id = c.Id
                    Name = c.Name
                    AltName = c.AlternativeName

                }
            )
    }
    |> Seq.toList

let printCustomers customers =
    for c in customers do
        match c.AltName with
        | None -> printfn $"Customer #%d{c.Id} %s{c.Name}"
        | Some altName -> printfn $"Customer #%d{c.Id} %s{c.Name} (%s{altName})"

let insertElton () =
    customers.Create("Elton") |> ignore
    ctx.SubmitUpdates()

let insertRobertAkaBob () =
    let row = customers.Create() // throw exception if I don't specify the non-null columns below
    row.Name <- "Robert"
    row.AlternativeName <- Some "Bob"
    ctx.SubmitUpdates()

let deleteEltonAndBob () =


    query {
        for c in customers do
            where (c.Name = "Elton" || c.AlternativeName = Some "Bob")
    }
    |> Seq.``delete all items from single table``
    |> ignore
    // |> Async.RunSynchronously // don't know how to use this construct so I'm ignoring + passing an (extra) unit instead

    ctx.SubmitUpdates

[<EntryPoint>]
let main _ =
    printfn "=== FSharp.Data.Sql demo ==="

    printfn "\n=> Listing customers at start..."
    listCustomers () |> printCustomers

    printfn "\n=> Inserting Elton..."
    insertElton ()
    printfn "\n=> Listing customers again..."
    listCustomers () |> printCustomers

    printfn "\n=> Inserting Robert (aka: Bob)..."
    insertRobertAkaBob ()
    printfn "\n=> Listing customers again..."
    listCustomers () |> printCustomers

    printfn "\n=> Deleting Elton and Bob..."
    deleteEltonAndBob () () // quircky, see my comment about `Async.RunSynchronously`
    printfn "\n=> Listing customers again..."
    listCustomers () |> printCustomers

    0
