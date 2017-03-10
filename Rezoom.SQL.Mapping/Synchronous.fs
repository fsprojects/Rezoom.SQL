namespace Rezoom.SQL.Synchronous
open System.Runtime.CompilerServices
open System.Threading
open System.Collections.Generic
open System.Data.Common
open Rezoom.SQL.Mapping

[<Extension>]
type Extensions =
    [<Extension>]
    static member Execute(cmd : Command<'a>, conn : DbConnection) =
        let token = CancellationToken.None
        let batch = CommandBatch(conn)
        let task = batch.Batch cmd token
        task.Result

    [<Extension>]
    static member Scalar(cmd : Command<#IScalar<_>>, conn : DbConnection) =
        cmd.Execute(conn).ScalarValue

    [<Extension>]
    static member TryExactlyOne(cmd : Command<#IReadOnlyList<_>>, conn : DbConnection) =
        let result = cmd.Execute(conn)
        if result.Count > 1 then
            failwith "Expected no more than one result from SQL command"
        elif result.Count = 0 then None
        else Some <| result.[0]

    [<Extension>]
    static member ExactlyOne(cmd : Command<#IReadOnlyList<_>>, conn : DbConnection) =
        cmd.Execute(conn) |> Seq.exactlyOne