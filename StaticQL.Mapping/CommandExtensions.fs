[<AutoOpen>]
module StaticQL.Mapping.CommandExtensions
open System
open System.Data
open System.Data.Common

type Command<'a> with
    member this.ExecuteTask(conn : DbConnection) =
        CommandBatch(conn).Batch(this)() |> Async.StartAsTask
    member this.ExecuteAsync(conn : DbConnection) =
        CommandBatch(conn).Batch(this)()
    member this.Execute(conn : DbConnection) =
        CommandBatch(conn).BatchSync(this)()