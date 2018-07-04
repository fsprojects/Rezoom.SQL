namespace Rezoom.SQL.Asynchronous
open System.Runtime.CompilerServices
open System.Threading
open System.Collections.Generic
open System.Data.Common
open FSharp.Control.Tasks.ContextInsensitive
open Rezoom.SQL
open Rezoom.SQL.Mapping

/// Extension methods for executing commands asynchronously against a database.
[<Extension>]
type Extensions =
    /// Execute the command on a connection and return its result sets.
    [<Extension>]
    static member Execute(cmd : Command<'a>, conn : DbConnection, token : CancellationToken) =
        task {
            let batch = AsyncCommandBatch(conn, tran = null)
            return! batch.Batch cmd token
        }
    
    /// Execute the command on a connection and return its result sets.
    [<Extension>]
    static member Execute(cmd : Command<'a>, conn : DbConnection) =
        cmd.Execute(conn, CancellationToken.None)

    /// Execute the command on a connection and return its result sets.
    /// The connection is obtained from the given `ConnectionContext` according to the command's `ConnectionName`
    /// property.
    [<Extension>]
    static member Execute(cmd : Command<'a>, context : ConnectionContext, token : CancellationToken) =
        cmd.Execute(context.GetConnection(cmd.ConnectionName), token)
    
    /// Execute the command on a connection and return its result sets.
    /// The connection is obtained from the given `ConnectionContext` according to the command's `ConnectionName`
    /// property.
    [<Extension>]
    static member Execute(cmd : Command<'a>, context : ConnectionContext) =
        cmd.Execute(context, CancellationToken.None)
    
    /// Execute the command on a connection and return its scalar result.
    [<Extension>]
    static member ExecuteScalar(cmd : Command<#IScalar<_>>, conn : DbConnection, token) =
        task {
            let! result = cmd.Execute(conn, token)
            return result.ScalarValue
        }

    /// Execute the command on a connection and return its scalar result.
    [<Extension>]
    static member ExecuteScalar(cmd : Command<#IScalar<_>>, conn : DbConnection) =
        cmd.ExecuteScalar(conn, CancellationToken.None)

    /// Execute the command on a connection and return its scalar result.
    /// The connection is obtained from the given `ConnectionContext` according to the command's `ConnectionName`
    /// property.
    [<Extension>]
    static member ExecuteScalar(cmd : Command<#IScalar<_>>, context : ConnectionContext, token) =
        cmd.ExecuteScalar(context.GetConnection(cmd.ConnectionName), token)

    /// Execute the command on a connection and return its scalar result.
    /// The connection is obtained from the given `ConnectionContext` according to the command's `ConnectionName`
    /// property.
    [<Extension>]
    static member ExecuteScalar(cmd : Command<#IScalar<_>>, context : ConnectionContext) =
        cmd.ExecuteScalar(context, CancellationToken.None)

    /// Execute the command on a connection and return the optional first and only row of its single result set.
    /// If the command returns more than 1 row, this throws an exception.
    [<Extension>]
    static member ExecuteTryExactlyOne(cmd : Command<#IReadOnlyList<_>>, conn : DbConnection, token) =
        task {
            let! result = cmd.Execute(conn, token)
            return
                if result.Count > 1 then
                    failwith "Expected no more than one result from SQL command"
                elif result.Count = 0 then None
                else Some <| result.[0]

        }

    /// Execute the command on a connection and return the optional first and only row of its single result set.
    /// If the command returns more than 1 row, this throws an exception.
    [<Extension>]
    static member ExecuteTryExactlyOne(cmd : Command<#IReadOnlyList<_>>, conn : DbConnection) =
        cmd.ExecuteTryExactlyOne(conn, CancellationToken.None)

    /// Execute the command on a connection and return the optional first and only row of its single result set.
    /// If the command returns more than 1 row, this throws an exception.
    /// The connection is obtained from the given `ConnectionContext` according to the command's `ConnectionName`
    /// property.
    [<Extension>]
    static member ExecuteTryExactlyOne(cmd : Command<#IReadOnlyList<_>>, context : ConnectionContext, token) =
        cmd.ExecuteTryExactlyOne(context.GetConnection(cmd.ConnectionName), token)

    /// Execute the command on a connection and return the optional first and only row of its single result set.
    /// If the command returns more than 1 row, this throws an exception.
    /// The connection is obtained from the given `ConnectionContext` according to the command's `ConnectionName`
    /// property.
    [<Extension>]
    static member ExecuteTryExactlyOne(cmd : Command<#IReadOnlyList<_>>, context : ConnectionContext) =
        cmd.ExecuteTryExactlyOne(context, CancellationToken.None)
    
    /// Execute the command on a connection and return the first and only row of its single result set.
    /// If the command returns no rows or more than 1 row, this throws an exception.
    [<Extension>]
    static member ExecuteExactlyOne(cmd : Command<#IReadOnlyList<_>>, conn : DbConnection, token) =
        task {
            let! result = cmd.Execute(conn, token)
            return result |> Seq.exactlyOne
        }

    /// Execute the command on a connection and return the first and only row of its single result set.
    /// If the command returns no rows or more than 1 row, this throws an exception.
    [<Extension>]
    static member ExecuteExactlyOne(cmd : Command<#IReadOnlyList<_>>, conn : DbConnection) =
        cmd.ExecuteExactlyOne(conn, CancellationToken.None)

    /// Execute the command on a connection and return the first and only row of its single result set.
    /// If the command returns no rows or more than 1 row, this throws an exception.
    /// The connection is obtained from the given `ConnectionContext` according to the command's `ConnectionName`
    /// property.
    [<Extension>]
    static member ExecuteExactlyOne(cmd : Command<#IReadOnlyList<_>>, context : ConnectionContext, token) =
        cmd.ExecuteExactlyOne(context.GetConnection(cmd.ConnectionName), token)

    /// Execute the command on a connection and return the first and only row of its single result set.
    /// If the command returns no rows or more than 1 row, this throws an exception.
    /// The connection is obtained from the given `ConnectionContext` according to the command's `ConnectionName`
    /// property.
    [<Extension>]
    static member ExecuteExactlyOne(cmd : Command<#IReadOnlyList<_>>, context : ConnectionContext) =
        cmd.ExecuteExactlyOne(context, CancellationToken.None)