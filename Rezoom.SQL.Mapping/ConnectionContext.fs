namespace Rezoom.SQL
open System
open System.Collections.Generic
open Rezoom.SQL.Mapping

/// Opens `DbConnection`s based on connection names.
/// Keeps them cached and open until it is disposed, then closes all its connections.
type ConnectionContext(provider : ConnectionProvider) =
    let connections = Dictionary(StringComparer.OrdinalIgnoreCase)
    new() = new ConnectionContext(DefaultConnectionProvider())
    member __.GetConnection(name : string) =
        let succ, found = connections.TryGetValue(name)
        if succ then found else
        let conn = provider.Open(name)
        connections.[name] <- conn
        conn
    member __.Dispose() =
        if connections.Count = 0 then ()
        elif connections.Count = 1 then
            for conn in connections.Values do conn.Dispose()
        else
            let exceptions = List()
            for conn in connections.Values do
                try
                    conn.Dispose()
                with
                | exn -> exceptions.Add(exn)
            if exceptions.Count = 0 then ()
            elif exceptions.Count = 1 then raise <| exceptions.[0]
            else raise <| AggregateException(exceptions)
    interface IDisposable with
        member this.Dispose() = this.Dispose()

