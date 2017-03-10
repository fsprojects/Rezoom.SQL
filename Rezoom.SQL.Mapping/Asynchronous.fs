namespace Rezoom.SQL.Asynchronous
open System.Runtime.CompilerServices
open System.Threading
open System.Collections.Generic
open System.Data.Common
open FSharp.Control.Tasks.ContextInsensitive
open Rezoom.SQL.Mapping

[<Extension>]
type Extensions =
    [<Extension>]
    static member Execute(cmd : Command<'a>, conn : DbConnection, token : CancellationToken) =
        task {
            let batch = CommandBatch(conn)
            return! batch.Batch cmd token
        }

    [<Extension>]
    static member Execute(cmd : Command<'a>, conn : DbConnection) =
        cmd.Execute(conn, CancellationToken.None)

    [<Extension>]
    static member Scalar(cmd : Command<#IScalar<_>>, conn : DbConnection, token) =
        task {
            let! result = cmd.Execute(conn, token)
            return result.ScalarValue
        }

    [<Extension>]
    static member Scalar(cmd : Command<#IScalar<_>>, conn : DbConnection) =
        cmd.Scalar(conn, CancellationToken.None)

    [<Extension>]
    static member TryExactlyOne(cmd : Command<#IReadOnlyList<_>>, conn : DbConnection, token) =
        task {
            let! result = cmd.Execute(conn, token)
            return
                if result.Count > 1 then
                    failwith "Expected no more than one result from SQL command"
                elif result.Count = 0 then None
                else Some <| result.[0]

        }

    [<Extension>]
    static member TryExactlyOne(cmd : Command<#IReadOnlyList<_>>, conn) =
        cmd.TryExactlyOne(conn, CancellationToken.None)

    [<Extension>]
    static member ExactlyOne(cmd : Command<#IReadOnlyList<_>>, conn : DbConnection, token) =
        task {
            let! result = cmd.Execute(conn, token)
            return result |> Seq.exactlyOne
        }

    [<Extension>]
    static member ExactlyOne(cmd : Command<#IReadOnlyList<_>>, conn : DbConnection) =
        cmd.ExactlyOne(conn, CancellationToken.None)