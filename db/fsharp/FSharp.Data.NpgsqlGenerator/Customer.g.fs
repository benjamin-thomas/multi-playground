namespace Generated.Generated

open Npgsql
open System
open System.Threading.Tasks
open System.Collections.Generic

type ICustomerRepository =
    abstract member GetCustomerByName: conn: NpgsqlConnection -> name: string -> Task<{| Id: int; Name: string; AlternativeName: string option; |} seq>



module CustomerRepository =


    let create () =
        { new ICustomerRepository with
            override this.GetCustomerByName (conn: NpgsqlConnection) (name: string) =
                use command = conn.CreateCommand()
                command.CommandText <- """SELECT * FROM customers WHERE name = @name;"""
                command.Parameters.Add(NpgsqlParameter(
                    ParameterName = "name",
                    DataTypeName = "text",
                    Value = name
                ))
                |> ignore
                task {
                    use! reader = command.ExecuteReaderAsync()
                    let result = ResizeArray<_>()
                    let mutable rowsRemain = false
                    let! rowsRemain2 = reader.ReadAsync()
                    rowsRemain <- rowsRemain2
                    while rowsRemain do
                        result.Add({|
                            Id = reader.GetInt32(0)
                            Name = reader.GetString(1)
                            AlternativeName =  
                                if reader.IsDBNull(2) then None
                                else Some(reader.GetString(2))
                        |})
                        let! rowsRemain2 = reader.ReadAsync()
                        rowsRemain <- rowsRemain2
                    
                    return result :> IEnumerable<_>
                }
        }

    let instance = create ()
