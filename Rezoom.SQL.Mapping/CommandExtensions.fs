[<AutoOpen>]
module Rezoom.SQL.Mapping.CommandExtensions
open System
open System.Collections.Generic
open System.Data
open System.Data.Common
open Rezoom

type CommandErrandArgument(parameters : (obj * DbType) IReadOnlyList) =
    member __.Parameters = parameters
    member __.Equals(other : CommandErrandArgument) =
        Seq.forall2 (=) parameters other.Parameters
    override __.GetHashCode() =
        let mutable h = 0
        for par, _ in parameters do
            h <- ((h <<< 5) + h) ^^^ par.GetHashCode()
        h
    override this.Equals(other : obj) =
        match other with
        | :? CommandErrandArgument as other -> this.Equals(other)
        | _ -> false

type CommandBatchFactory() =
    inherit ServiceFactory<CommandBatch>()
    override __.CreateService(cxt) = failwith "not implemented" // TODO
    override __.DisposeService(svc) = ()
    override __.ServiceLifetime = ServiceLifetime.StepLocal

type CommandErrand<'a>(command : Command<'a>) =
    inherit AsynchronousErrand<'a>()
    let cacheArgument = CommandErrandArgument(command.Parameters)
    override __.CacheInfo = command.CacheInfo
    override __.CacheArgument = box cacheArgument 
    override __.SequenceGroup = box typeof<Command>
    override __.Prepare(cxt) =
        let batch = cxt.GetService<CommandBatchFactory, _>()
        batch.Batch(command)

type Command<'a> with
    member this.ExecutePlan() =
        CommandErrand(this) |> Plan.ofErrand
    member this.ExecuteAsync(conn : DbConnection) =
        CommandBatch(conn).Batch(this)()
