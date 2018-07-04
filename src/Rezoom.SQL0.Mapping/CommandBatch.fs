namespace Rezoom.SQL.Mapping
open System
open System.Data
open System.Data.Common
open System.Collections.Generic
open System.Text
open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks.ContextInsensitive
open Rezoom.SQL

type private CommandBatchRuntimeBackend =
    private
    | SQLServer
    | Oracle
    | Postgres
    | MySQL
    | SQLite
    | Other
    static member OfNamespace(ns : string) =
        match ns with
        | "System.Data.SqlClient" -> SQLServer
        | "System.Data.OracleClient"
        | "Oracle.DataAccess.Client" -> Oracle
        | "Npgsql" -> Postgres
        | "MySql.Data.MySqlClient" -> MySQL
        | "System.Data.SQLite"
        | "Devart.Data.SQLite"
        | "Microsoft.Data.Sqlite"-> SQLite
        | _ -> Other
    member this.MaxParameters() =
        match this with
        | SQLServer -> 2098 // actual is 2100 but SqlCommand implementation takes 2 for itself
        | Oracle -> 1900 // actual is 2000 but leave plenty of breathing room since it's untested
        | Postgres
        | MySQL -> 10_000
        | SQLite 
        | Other -> 999
    static member private PgType(ty : DbType) =
        match ty with
        | DbType.String
        | DbType.StringFixedLength
        | DbType.AnsiString
        | DbType.AnsiStringFixedLength -> "text"
        | DbType.Byte
        | DbType.SByte
        | DbType.UInt16
        | DbType.UInt32
        | DbType.UInt64
        | DbType.Int16
        | DbType.Int32
        | DbType.Int64
        | DbType.VarNumeric
        | DbType.Decimal
        | DbType.Single
        | DbType.Double -> "numeric"
        | DbType.Binary -> "bytea"
        | DbType.Guid -> "uuid"
        | DbType.DateTime
        | DbType.DateTime2
        | DbType.DateTimeOffset -> "timestamptz"
        | _ -> "unknown"
    member this.EmptyInList(ty : DbType) =
        match this with
        | Postgres ->
            // PG has to be difficult and demand a type specifier matching the input
            "(SELECT NULL::" + CommandBatchRuntimeBackend.PgType(ty) + " WHERE FALSE)"
        | SQLite ->
            // SQLite is cool and accepts the simple approach. This might be faster than the empty subquery.
            "()"
        | _ ->
            "(SELECT NULL WHERE 1=0)"

type private CommandBatchBuilder(conn : DbConnection, tran : DbTransaction) =
    let runtimeBackend = CommandBatchRuntimeBackend.OfNamespace(conn.GetType().Namespace)
    let maxParameters = runtimeBackend.MaxParameters()

    static let terminatorColumn i = "RZSQL_TERMINATOR_" + string i
    static let terminator i = ";--'*/;SELECT NULL AS " + terminatorColumn i
    static let parameterName i = "@RZSQL_" + string i
    static let parameterNameArray i j = "@RZSQL_" + string i + "_" + string j
    static let dynamicParameterName i = "@RZSQL_INLINE_" + string i
    static let localName i name = "RZSQL_" + name + "_" + string i
    let commands = ResizeArray<Command>()
    let mutable parameterCount = 0
    let mutable evaluating = false

    let addCommand (builder : StringBuilder) (dbCommand : DbCommand) (commandIndex : int) (command : Command) =
        let parameterOffset = dbCommand.Parameters.Count
        let addParam name dbType (value : obj) =
            let dbParam = dbCommand.CreateParameter()
            dbParam.ParameterName <- name
            dbParam.DbType <- dbType
            dbParam.Value <- if isNull value then box DBNull.Value else value
            ignore <| dbCommand.Parameters.Add(dbParam)
        for i, parameter in command.Parameters |> Seq.indexed do
            match parameter with
            | ListParameter(parameterType, os) ->
                let mutable j = 0
                for elem in os do
                    addParam (parameterNameArray (parameterOffset + i) j) parameterType elem
                    j <- j + 1
            | ScalarParameter(parameterType, o) ->
                addParam (parameterName (parameterOffset + i)) parameterType o
            | RawSQLParameter _ -> ()
        let rec addFragment fragment =
            let fragmentString =
                match fragment with
                | LocalName name -> localName commandIndex name
                | CommandText str -> str
                | Parameter i ->
                    match command.Parameters.[i] with
                    | ListParameter(dbTy, os) ->
                        if os.Length = 0 then
                            runtimeBackend.EmptyInList(dbTy)
                        else
                            let parNames =
                                seq {
                                    for j = 0 to os.Length - 1 do yield parameterNameArray (parameterOffset + i) j
                                }
                            "(" + String.concat "," parNames + ")"
                    | ScalarParameter _ -> parameterName (parameterOffset + i)
                    | RawSQLParameter frags ->
                        for frag in frags do
                            addFragment frag
                        ""
                | InlineParameter (dbType, value) ->
                    let name = dynamicParameterName dbCommand.Parameters.Count
                    addParam name dbType value
                    name
                | Indent | Outdent -> ""
                | Whitespace -> " "
                | LineBreak -> "\n"
            ignore <| builder.Append(fragmentString)
        for fragment in command.Fragments do
            addFragment fragment
        match command.ResultSetCount with
        | Some _ -> () // no need to add terminator statement
        | None when commandIndex + 1 >= commands.Count -> ()
        | None ->
            builder.Append(terminator commandIndex) |> ignore
    let buildCommand (dbCommand : DbCommand) =
        dbCommand.Transaction <- tran
        let builder = StringBuilder()
        for commandIndex, command in commands |> Seq.indexed do
            addCommand builder dbCommand commandIndex command
        dbCommand.CommandText <- builder.ToString()

    member __.BatchCommand(cmd : Command) =
        let countInlineParameters fragments =
            let mutable i = 0
            for fragment in fragments do
                match fragment with
                | InlineParameter _ -> i <- i + 1
                | _ -> ()
            i

        let mutable count = countInlineParameters cmd.Fragments
        for par in cmd.Parameters do
            count <- count +
                match par with
                | ListParameter (_, os) -> os.Length
                | ScalarParameter _ -> 1
                | RawSQLParameter frags -> countInlineParameters frags

        if parameterCount + count > maxParameters then
            Nullable()
        else
            let index = commands.Count
            commands.Add(cmd)
            parameterCount <- parameterCount + count
            Nullable(index)

    member __.EvaluateSync() =
        if evaluating then failwith "Already evaluating command"
        else evaluating <- true
        use dbCommand = conn.CreateCommand()
        buildCommand dbCommand
        use reader = dbCommand.ExecuteReader()
        let reader = reader : DbDataReader
        let processed = ResizeArray()
        for i = 0 to commands.Count - 1 do
            let cmd = commands.[i]
            let processor = cmd.ObjectResultSetProcessor()
            let mutable resultSetCount = match cmd.ResultSetCount with | Some 0 -> -1 | _ -> 0
            while resultSetCount >= 0 do
                processor.BeginResultSet(reader)
                let mutable hasRows = true
                while hasRows do
                    let hasRow = reader.Read()
                    if hasRow then
                        processor.ProcessRow()
                    else
                        hasRows <- false
                resultSetCount <- resultSetCount + 1
                let hasNextResult = reader.NextResult()
                match cmd.ResultSetCount with
                | None -> // check for terminator
                    if not hasNextResult || reader.FieldCount = 1 && reader.GetName(0) = terminatorColumn i then
                        resultSetCount <- -1
                    else
                        let hasNextResult = reader.NextResult()
                        if not hasNextResult then
                            resultSetCount <- -1
                | Some count ->
                    if resultSetCount = count then 
                        resultSetCount <- -1
                    elif not hasNextResult then
                        failwithf
                            "Command claimed it would produce %d result sets, but only yielded %d"
                            count resultSetCount
            processed.Add(processor.ObjectGetResult())
        processed

    member __.EvaluateAsync() =
        if evaluating then failwith "Already evaluating command"
        else evaluating <- true
        task {
            use dbCommand = conn.CreateCommand()
            buildCommand dbCommand
            use! reader = dbCommand.ExecuteReaderAsync()
            let reader = reader : DbDataReader
            let processed = ResizeArray()
            for i = 0 to commands.Count - 1 do
                let cmd = commands.[i]
                let processor = cmd.ObjectResultSetProcessor()
                let mutable resultSetCount = match cmd.ResultSetCount with | Some 0 -> -1 | _ -> 0
                while resultSetCount >= 0 do
                    processor.BeginResultSet(reader)
                    let mutable hasRows = true
                    while hasRows do
                        let! hasRow = reader.ReadAsync()
                        if hasRow then
                            processor.ProcessRow()
                        else
                            hasRows <- false
                    resultSetCount <- resultSetCount + 1
                    let! hasNextResult = reader.NextResultAsync()
                    match cmd.ResultSetCount with
                    | None -> // check for terminator
                        if not hasNextResult || reader.FieldCount = 1 && reader.GetName(0) = terminatorColumn i then
                            resultSetCount <- -1
                        else
                            let! hasNextResult = reader.NextResultAsync()
                            if not hasNextResult then
                                resultSetCount <- -1
                    | Some count ->
                        if resultSetCount = count then 
                            resultSetCount <- -1
                        elif not hasNextResult then
                            failwithf
                                "Command claimed it would produce %d result sets, but only yielded %d"
                                count resultSetCount
                processed.Add(processor.ObjectGetResult())
            return processed
        }

module private CommandBatchUtilities =
    let inline build
        (builders : CommandBatchBuilder ResizeArray)
        (conn : DbConnection)
        (tran : DbTransaction)
        (cmd : #Command<'a>)
        retrieveResult
        =
        let builderIndex = builders.Count - 1
        let resultsIndex = builders.[builderIndex].BatchCommand(cmd)
        if resultsIndex.HasValue then
            retrieveResult builderIndex resultsIndex.Value
        else
            let next = CommandBatchBuilder(conn, tran)
            let builderIndex = builderIndex + 1
            let resultsIndex = next.BatchCommand(cmd)
            builders.Add(next)
            if resultsIndex.HasValue then
                retrieveResult builderIndex resultsIndex.Value
            else
                failwith "Command has too many parameters to run"
open CommandBatchUtilities

type AsyncCommandBatch(conn : DbConnection, tran : DbTransaction) =
    let deferred = Queue<unit -> unit>()
    let builders = ResizeArray<CommandBatchBuilder>()
    let evaluation =
        lazy
            while deferred.Count > 0 do
                deferred.Dequeue()()
            task {
                let arr = Array.zeroCreate builders.Count
                for i = 0 to builders.Count - 1 do
                    let! resultSets = builders.[i].EvaluateAsync()
                    arr.[i] <- resultSets
                return arr
            }
    do
        builders.Add(CommandBatchBuilder(conn, tran))
    member this.Batch(f : unit -> #Command<'a>) : (CancellationToken -> 'a Task) =
        let mutable eventuallyBatched = None
        deferred.Enqueue(fun () ->
            let cmd = f()
            let batched = this.Batch(cmd)
            eventuallyBatched <- Some batched)
        fun (token : CancellationToken) ->
            task {
                let! _ = evaluation.Value
                match eventuallyBatched with
                | None -> return failwith "BUG: deferred batch didn't work"
                | Some batched ->
                    return! batched token
            }
    member __.Batch(cmd : #Command<'a>) : (CancellationToken -> 'a Task) =
        let inline retrieveResult builderIndex resultsIndex =
            fun (_ : CancellationToken) ->
                task {
                    let! result = evaluation.Value
                    let boxed = result.[builderIndex].[resultsIndex]
                    return (Unchecked.unbox boxed : 'a)
                }
        build builders conn tran cmd retrieveResult

type SyncCommandBatch(conn : DbConnection, tran : DbTransaction) =
    let builders = ResizeArray<CommandBatchBuilder>()
    let evaluation =
        lazy
            let arr = Array.zeroCreate builders.Count
            for i = 0 to builders.Count - 1 do
                let resultSets = builders.[i].EvaluateSync()
                arr.[i] <- resultSets
            arr
    do
        builders.Add(CommandBatchBuilder(conn, tran))
    member __.Batch(cmd : #Command<'a>) =
        let inline retrieveResult builderIndex resultsIndex =
            fun () ->
                let arrs = evaluation.Value
                arrs.[builderIndex].[resultsIndex] |> Unchecked.unbox : 'a
        build builders conn tran cmd retrieveResult

