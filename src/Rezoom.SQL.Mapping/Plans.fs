namespace Rezoom.SQL.Plans
open System.Runtime.CompilerServices
open System
open System.Collections.Generic
open System.Data
open System.Data.Common
open Rezoom
open Rezoom.SQL
open Rezoom.SQL.Mapping
open FSharp.Control.Tasks.ContextInsensitive
open System.Threading

type private ExecutionLocalConnections(provider : ConnectionProvider) =
    let connections = Dictionary()
    member __.GetConnection(name) : DbConnection * DbTransaction =
        let succ, tuple = connections.TryGetValue(name)
        if succ then tuple else
        let conn = provider.Open(name)
        let tran = provider.BeginTransaction(conn)
        let tuple = conn, tran
        connections.Add(name, tuple)
        tuple
    member __.Dispose(state) = 
        let mutable exn = null
        for conn, tran in connections.Values do
            try
                match state with
                | ExecutionSuccess ->
                    try
                        tran.Commit()
                    with
                    | e ->
                        if isNull exn then exn <- e
                        else exn <- AggregateException(exn, e)
                | ExecutionFault ->
                    () // don't explicitly rollback, tran.Dispose() should handle it
                try
                    tran.Dispose()
                finally
                    conn.Dispose()
            with
            | e ->
                if isNull exn then exn <- e
                else exn <- AggregateException(exn, e)
        connections.Clear()
        if not (isNull exn) then raise exn
    // don't implement IDisposable because we need exec. state to know how to end transactions

type private ExecutionLocalConnectionsFactory() =
    inherit ServiceFactory<ExecutionLocalConnections>()
    override __.ServiceLifetime = ServiceLifetime.ExecutionLocal
    override __.CreateService(cxt) =
        let provider = 
            match cxt.Configuration.TryGetConfig<ConnectionProvider>() with
            | None -> DefaultConnectionProvider() :> ConnectionProvider
            | Some provider -> provider
        ExecutionLocalConnections(provider)
    override __.DisposeService(state, svc) = svc.Dispose(state)

type private StepLocalBatches(conns : ExecutionLocalConnections) =
    let batches = Dictionary()
    member __.GetBatch(name) =
        let succ, batch = batches.TryGetValue(name)
        if succ then batch else
        let conn, tran = conns.GetConnection(name)
        let batch = AsyncCommandBatch(conn, tran)
        batches.Add(name, batch)
        batch

type private StepLocalBatchesFactory() =
    inherit ServiceFactory<StepLocalBatches>()
    override __.ServiceLifetime = ServiceLifetime.StepLocal
    override __.CreateService(cxt) = StepLocalBatches(cxt.GetService<ExecutionLocalConnectionsFactory, _>())
    override __.DisposeService(_, _) = ()

type private CommandErrandArgument(parameters : CommandParameter IReadOnlyList) =
    member __.Parameters = parameters
    member __.Equals(other : CommandErrandArgument) =
        Seq.forall2 (=) parameters other.Parameters
    override __.GetHashCode() =
        let mutable h = 0
        for par in parameters do
            h <- ((h <<< 5) + h) ^^^ hash par
        h
    override this.Equals(other : obj) =
        match other with
        | :? CommandErrandArgument as other -> this.Equals(other)
        | _ -> false

type private CommandErrand<'a>(command : Command<'a>) =
    inherit AsynchronousErrand<'a>()
    let cacheArgument = CommandErrandArgument(command.Parameters)
    override __.CacheInfo = command.CacheInfo
    override __.CacheArgument = box cacheArgument 
    override __.SequenceGroup = null
    override __.Prepare(cxt) =
        let batches = cxt.GetService<StepLocalBatchesFactory, _>()
        batches.GetBatch(command.ConnectionName).Batch(command)
    override __.ToString() =
        let all = CommandFragment.Stringize(command.Fragments)
        let truncate = 80
        if all.Length < truncate then all else all.Substring(0, truncate - 3) + "..."

type private SharedCommandStepState<'id, 'a when 'id : equality>(factory : SharedCommandFactory<'id, 'a>, batch : AsyncCommandBatch) =
    let ids = ResizeArray<'id>()
    // defer the command-building till the last possible moment before the batch executes
    let bulkTask = batch.Batch(fun () -> factory.BuildCommand(ids))
    let lazyResults =
        lazy
            task {
                let! resultSet = bulkTask CancellationToken.None
                let dict = Dictionary()
                for resultRow in resultSet do
                    let id = factory.Selector(resultRow)
                    let succ, found = dict.TryGetValue(id)
                    let found =
                        if succ then found else
                        let it = ResizeArray()
                        dict.[id] <- it
                        it
                    found.Add(resultRow)
                return dict
            }
    member this.PrepareId(id : 'id) =
        ids.Add(id)
        fun (_ : CancellationToken) ->
            task {
                let! results = lazyResults.Value
                let succ, found = results.TryGetValue(id)
                return
                    if succ then found :> 'a IReadOnlyList
                    else [||] :> 'a IReadOnlyList
            }

and private SharedCommandStepStateLookup<'id, 'a when 'id : equality>() =
    let idsByFactory = Dictionary<obj, SharedCommandStepState<'id, 'a>>()
    member this.ByFactory(factory : SharedCommandFactory<'id, 'a>, batch : AsyncCommandBatch) =
        let succ, found = idsByFactory.TryGetValue(factory)
        if succ then found else
        let state = SharedCommandStepState<'id, 'a>(factory, batch)
        idsByFactory.[factory] <- state
        state

and private SharedCommandStepStateLookupFactory<'id, 'a when 'id : equality>() =
    inherit ServiceFactory<SharedCommandStepStateLookup<'id, 'a>>()
    override __.ServiceLifetime = ServiceLifetime.StepLocal
    override __.CreateService(_) = SharedCommandStepStateLookup<'id, 'a>()
    override __.DisposeService(_, _) = ()

and SharedCommandFactory<'id, 'a when 'id : equality>(buildCommand : 'id seq -> Command<'a IReadOnlyList>, selector : 'a -> 'id) =
    let templateCommand = buildCommand Seq.empty
    let connectionName = templateCommand.ConnectionName
    let cacheArgument = CommandErrandArgument(templateCommand.Parameters)
    member internal __.BuildCommand = buildCommand
    member internal __.Selector = selector
    member factory.ErrandForKey(id : 'id) =
        let cacheArg = box (id, cacheArgument)
        { new AsynchronousErrand<'a IReadOnlyList>() with
            override __.CacheInfo = templateCommand.CacheInfo
            override __.CacheArgument = cacheArg
            override __.SequenceGroup = null
            override __.ToString() =
                templateCommand.ToString() + " (Arg = " + string (box id) + ")"
            override __.Prepare(cxt) =
                let batches = cxt.GetService<StepLocalBatchesFactory, _>()
                let batch = batches.GetBatch(connectionName)
                let subErrands = cxt.GetService<SharedCommandStepStateLookupFactory<'id, 'a>, _>().ByFactory(factory, batch)
                subErrands.PrepareId(id)
        } :> Errand<'a IReadOnlyList>

// Have to use a C#-style extension method to support the scalar constraint.

[<Extension>]
type ScalarCommandExtensions =
    [<Extension>]
    static member Plan(cmd : Command<'a>) =
        CommandErrand(cmd) |> Plan.ofErrand

    [<Extension>]
    static member Scalar(cmd : Command<#IScalar<_>>) =
        plan {
            let! planResult = cmd.Plan()
            return planResult.ScalarValue
        }

    [<Extension>]
    static member TryExactlyOne(cmd : Command<#IReadOnlyList<_>>) =
        plan {
            let! planResult = cmd.Plan()
            return
                if planResult.Count > 1 then
                    failwith "Expected no more than one result from SQL command"
                elif planResult.Count = 0 then None
                else Some <| planResult.[0]
        }

    [<Extension>]
    static member ExactlyOne(cmd : Command<#IReadOnlyList<_>>) =
        plan {
            let! planResult = cmd.Plan()
            return
                if planResult.Count <> 1 then
                    failwith "Expected exactly one result from SQL command"
                else
                    planResult.[0]
        }
