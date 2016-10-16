namespace StaticQL.Mapping
open System
open System.Data
open System.Data.Common
open System.Collections.Generic
open System.Text
open System.Threading.Tasks

type private CommandBatchBuilder(conn : DbConnection) =
    static let terminatorColumn i = "STATICQL_TERMINATOR_" + string i
    static let terminator i = ";--'*/;SELECT NULL AS " + terminatorColumn i
    static let parameterName i = "@STATICQL_" + string i
    static let localName i name = "STATICQL_" + name + "_" + string i
    let commands = ResizeArray<Command>()
    let mutable evaluating = false
    let mutable result = None

    let buildCommand (dbCommand : DbCommand) =
        let builder = StringBuilder()
        for commandIndex, command in commands |> Seq.indexed do
            let parameterOffset = dbCommand.Parameters.Count
            for parameterValue, parameterType in command.Parameters do
                let dbParam = dbCommand.CreateParameter()
                dbParam.ParameterName <- parameterName dbCommand.Parameters.Count
                dbParam.DbType <- parameterType
                dbParam.Value <- parameterValue
                ignore <| dbCommand.Parameters.Add(dbParam)
            for fragment in command.Fragments do
                builder.Append
                    ( match fragment with
                    | LocalName name -> localName commandIndex name
                    | CommandText str -> str
                    | Parameter i -> parameterName (parameterOffset + i)
                    | Whitespace -> " ") |> ignore
            match command.ResultSetCount with
            | Some _ -> () // no need to add terminator statement
            | None when commandIndex + 1 >= commands.Count -> ()
            | None ->
                builder.Append(terminator commandIndex) |> ignore
        dbCommand.CommandText <- builder.ToString()

    member __.Evaluate() =
        async {
            match result with // if one ran synchronously we need to pull that result
            | Some r -> return r
            | None ->
            if evaluating then failwith "Already evaluating command"
            else evaluating <- true
            use dbCommand = conn.CreateCommand()
            buildCommand dbCommand
            use! reader = Async.AwaitTask <| dbCommand.ExecuteReaderAsync()
            let processed = ResizeArray()
            for i = 0 to commands.Count - 1 do
                let cmd = commands.[i]
                let processor = cmd.ObjectResultSetProcessor()
                let mutable resultSetCount = match cmd.ResultSetCount with | Some 0 -> -1 | _ -> 0
                while resultSetCount >= 0 do
                    processor.BeginResultSet(reader)
                    let mutable hasRows = true
                    while hasRows do
                        let! hasRow = Async.AwaitTask <| reader.ReadAsync()
                        if hasRow then
                            processor.ProcessRow()
                        else
                            hasRows <- false
                    resultSetCount <- resultSetCount + 1
                    let! hasNextResult = Async.AwaitTask <| reader.NextResultAsync()
                    match cmd.ResultSetCount with
                    | None -> // check for terminator
                        if not hasNextResult || reader.FieldCount = 1 && reader.GetName(0) = terminatorColumn i then
                            resultSetCount <- -1
                        else
                            let! hasNextResult = Async.AwaitTask <| reader.NextResultAsync() // skip over terminator
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

    member __.BatchCommand(cmd) =
        let index = commands.Count
        commands.Add(cmd)
        index

type CommandBatch(conn : DbConnection) =
    let builder = CommandBatchBuilder(conn)
    let evaluation = lazy Async.RunSynchronously(builder.Evaluate())
    member __.Batch(cmd : #Command<'a>) =
        let index = builder.BatchCommand(cmd)
        fun () ->
            evaluation.Value.[index] |> Unchecked.unbox : 'a

type AsyncCommandBatch(conn : DbConnection) =
    let builder = CommandBatchBuilder(conn)
    let evaluation = lazy Async.StartAsTask(builder.Evaluate())
    member __.Batch(cmd : #Command<'a>) =
        let index = builder.BatchCommand(cmd)
        fun () ->
            evaluation.Value.ContinueWith
                ( (fun (t : _ ResizeArray Task) -> t.Result.[index] |> Unchecked.unbox : 'a)
                , TaskContinuationOptions.ExecuteSynchronously
                )
