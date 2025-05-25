#r "nuget: Newtonsoft.Json, 13.0.1"

open System
open Newtonsoft.Json

(*

clear && dotnet fsi ./main.fsx

*)

printfn "[%s] Log example (in current time zone)" (DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss"))
printfn "[%s] Log example (in UTC)" (DateTime.UtcNow.ToString("yyyy-MM-dd HH:mm:ss"))

type TestData = { Name: string; Date: DateTime }

let data = { Name = "Test"; Date = DateTime.Now }
let json = JsonConvert.SerializeObject(data, Formatting.Indented)
printfn "---"
printfn "Serialize..."
printfn "%s" json

printfn "---"
printfn "Deserialize..."
let obj = JsonConvert.DeserializeObject<TestData>(json)
printfn "Name: %s" obj.Name
printfn "---"