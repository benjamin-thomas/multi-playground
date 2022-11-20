// See https://aka.ms/new-console-template for more information

/*
dotnet ef dbcontext scaffold "Host=pg.mpg.test;Database=mpg_db;Username=postgres;Password=postgres" Npgsql.EntityFrameworkCore.PostgreSQL
dotnet watch run NpgsqlEfCoreDemo
 */

namespace NpgsqlEfCoreDemo;

internal static class Program
{
    public static void Main()
    {
        Console.WriteLine("=> Entity Framework Core demo...");

        using var db = new mpg_dbContext();
        var customers = db.Customers
            .Where(b => b.Id < 10)
            .OrderBy(b => b.Name) // how to order by nulls first/last?
            .ToList();

        foreach (var c in customers)
        {
            /*
                I get this warning in Rider + vscode (line 35):
                    "Dereference of a possibly null reference"

                I can get an exception:
                    "Unhandled exception. System.NullReferenceException:
                         Object reference not set to an instance of an object."
             */
            Console.WriteLine(
                $"ID      = {c.Id}\n" +
                $"Name    = {c.Name} ({c.Name.Length})\n" +
                $"AltName = {c.AlternativeName} ({c.AlternativeName.Length})\n");
        }
    }
}