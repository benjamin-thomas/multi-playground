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
    "Host=pg.hello.test;Database=mpg_db;UserName=postgres;Password=postgres;"

(*
    Requires: Npgsql.dll => `Npgsql` package
*)
type Db =
    SqlDataProvider<DatabaseVendor=Common.DatabaseProviderTypes.POSTGRESQL, ConnectionString=connStr, UseOptionTypes=Common.NullableColumnType.OPTION>

let ctx = Db.GetDataContext()

type Customer =
    {
        Id: int
        Name: string
        AltName: string option
    }

let listCustomers =
    query {
        for c in ctx.Public.Customers do
            where (c.Id < 10)

            select (
                {
                    Id = c.Id
                    Name = c.Name
                    AltName = c.AlternativeName

                }
            )
    }
    |> Seq.toList

[<EntryPoint>]
let main _ =
    printfn "=== FSharp.Data.Sql demo ==="
    let customers = listCustomers

    for c in customers do
        match c.AltName with
        | None -> printfn $"Customer #%d{c.Id} %s{c.Name}"
        | Some altName -> printfn $"Customer #%d{c.Id} %s{c.Name} (%s{altName})"

    0
