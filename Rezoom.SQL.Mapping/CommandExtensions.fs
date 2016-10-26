[<AutoOpen>]
module Rezoom.SQL.Mapping.CommandExtensions
open System
open System.Data
open System.Data.Common

type Command<'a> with
    member this.ExecuteAsync(conn : DbConnection) =
        AsyncCommandBatch(conn).Batch(this)()
    member this.Execute(conn : DbConnection) =
        CommandBatch(conn).Batch(this)()