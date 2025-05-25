#:package Newtonsoft.Json@13.0.*

using Newtonsoft.Json;

/*

dotnet 10 (preview) allows running files in a script-like fashion:
  https://www.youtube.com/watch?v=98MizuB7i-w

Doesn't seem to work with F# currently. Use `dotnet fsi ./main.fsx` for now.


./activate_dotnet10_preview.sh
code .
clear && dotnet run ./main.cs

 */

Console.WriteLine($"[{DateTime.Now:yyyy-MM-dd HH:mm:ss}] Log example (in current time zone)");
Console.WriteLine($"[{DateTime.UtcNow:yyyy-MM-dd HH:mm:ss}] Log example (in UTC)");

var data = new { Name = "Test", Date = DateTime.Now };
string json = JsonConvert.SerializeObject(data, Formatting.Indented);
Console.WriteLine("---");
Console.WriteLine("Serialize...");
Console.WriteLine(json);

Console.WriteLine("---");
Console.WriteLine("Deserialize...");
var obj = JsonConvert.DeserializeObject<dynamic>(json) ?? throw new Exception("Failed to deserialize");
Console.WriteLine($"Name: {obj.Name}");
Console.WriteLine("---");