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


let insertNewCustomer x = customers.Create($"NAME_FOR_X[%d{x}]")

let insertManyCustomers (count: int) () =
    for x in 1..count do
        insertNewCustomer x |> ignore

    ctx.SubmitUpdates()

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

    // Testing perf (Release build)
    // Initial 1s for the program to get up to this point.
    // 1s + 6s --> that's slow! I'm opening/closing a transaction every time here
    // 1s + 1.7s --> flushing the ctx at the end of the loop, still slow!
    (*
        mpg_db=# INSERT INTO customers (name) SELECT 'NAME_FOR_X[' || x || ']' FROM generate_series(1, 1000)x;
        INSERT 0 1000
        Time: 14.186 ms


Best-case scenario (commiting at the end):

time ./bin/Release/net6.0/SQLProviderDemo

=> Inserting 1000 customers...

real    0m2.626s
user    0m2.240s
sys     0m0.339s

    *)
    printfn "\n=> Inserting 1000 customers..."
    insertManyCustomers 1000 ()
    // DELETE FROM customers WHERE name LIKE 'NAME_FOR_X%';

    0
