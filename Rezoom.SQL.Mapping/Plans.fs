namespace Rezoom.SQL.Plans
open System.Runtime.CompilerServices
open System
open System.Configuration
open System.Collections.Generic
open System.Data
open System.Data.Common
open Rezoom
open Rezoom.SQL.Mapping

[<AbstractClass>]
type ConnectionProvider() =
    abstract member Open : name : string -> DbConnection
    abstract member BeginTransaction : DbConnection -> DbTransaction
    default __.BeginTransaction(conn) = conn.BeginTransaction()

type DefaultConnectionProvider() =
    inherit ConnectionProvider()
    static member ResolveConnectionString(name : string) =
        let connectionStrings = ConfigurationManager.ConnectionStrings
        if isNull connectionStrings then
            failwith "No <connectionStrings> element in config"
        let connectionString = connectionStrings.[name]
        if isNull connectionString then
            failwith "No connection string by the expected name"
        else
            connectionString
    override __.Open(name) =
        let connectionString = DefaultConnectionProvider.ResolveConnectionString(name)
        let provider = DbProviderFactories.GetFactory(connectionString.ProviderName)
        let conn = provider.CreateConnection()
        conn.ConnectionString <- connectionString.ConnectionString
        conn.Open()
        conn

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
                | ExecutionSuccess -> tran.Commit()
                | ExecutionFault -> // don't explicitly rollback, tran.Dispose() should handle it
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
        let batch = CommandBatch(conn, tran)
        batches.Add(name, batch)
        batch

type private StepLocalBatchesFactory() =
    inherit ServiceFactory<StepLocalBatches>()
    override __.ServiceLifetime = ServiceLifetime.StepLocal
    override __.CreateService(cxt) = StepLocalBatches(cxt.GetService<ExecutionLocalConnectionsFactory, _>())
    override __.DisposeService(_, _) = ()

type private CommandErrandArgument(parameters : (obj * DbType) IReadOnlyList) =
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
