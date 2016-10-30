[<AutoOpen>]
module Rezoom.SQL.Mapping.CommandExtensions
open System
open System.Data
open System.Data.Common
open Rezoom

type Command<'a> with
    member this.ExecuteAsync(conn : DbConnection) =
        CommandBatch(conn).Batch(this)()
    member this.Execute(conn : DbConnection) =
        CommandBatch(conn).Batch(this)()

type CommandErrand<'a>(command : Command<'a>) =
    inherit AsynchronousErrand<'a>()
    override __.CacheInfo = command.CacheInfo
    override __.CacheArgument =
        let parameters = command.Parameters
        { new obj() with
            override __.GetHashCode() = 0
                
        }
    override __.SequenceGroup = box typeof<Command>
    override __.Prepare(cxt) =
        let batch = cxt.GetService<CommandBatch>()
        batch.Batch(command)