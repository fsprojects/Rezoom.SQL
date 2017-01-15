namespace Rezoom.SQL.Mapping
open System
open System.Data
open System.Data.Common
open System.Collections.Generic
open System.Text
open System.Threading.Tasks
open FSharp.Control.Tasks.ContextInsensitive

type private CommandBatchBuilder(conn : DbConnection) =
    static let terminatorColumn i = "RZSQL_TERMINATOR_" + string i
    static let terminator i = ";--'*/;SELECT NULL AS " + terminatorColumn i
    static let parameterName i = "@RZSQL_" + string i
    static let parameterNameArray i j = "@RZSQL_" + string i + "_" + string j
    static let localName i name = "RZSQL_" + name + "_" + string i
    let commands = ResizeArray<Command>()
    let mutable evaluating = false

    let addCommand (builder : StringBuilder) (dbCommand : DbCommand) (commandIndex : int) (command : Command) =
        let parameterOffset = dbCommand.Parameters.Count
        let addParam name dbType value =
            let dbParam = dbCommand.CreateParameter()
            dbParam.ParameterName <- name
            dbParam.DbType <- dbType
            dbParam.Value <- value
            ignore <| dbCommand.Parameters.Add(dbParam)
        for i, (parameterValue, parameterType) in command.Parameters |> Seq.indexed do
            match parameterValue with
            | :? Array as arr ->
                let mutable j = 0
                for elem in arr do
                    addParam (parameterNameArray (parameterOffset + i) j) parameterType parameterValue
                    j <- j + 1
            | _ ->
                addParam (parameterName (parameterOffset + 1)) parameterType parameterValue
        for fragment in command.Fragments do
            let fragmentString =
                match fragment with
                | LocalName name -> localName commandIndex name
                | CommandText str -> str
                | Parameter i ->
                    match command.Parameters.[i] |> fst with
                    | :? Array as arr ->
                        let parNames =
                            seq {
                                for j = 0 to arr.Length - 1 do yield parameterNameArray i j
                            }
                        "(" + String.concat "," parNames + ")"
                    | _ -> parameterName (parameterOffset + i)
                | Whitespace -> " "
            ignore <| builder.Append(fragmentString)
        match command.ResultSetCount with
        | Some _ -> () // no need to add terminator statement
        | None when commandIndex + 1 >= commands.Count -> ()
        | None ->
            builder.Append(terminator commandIndex) |> ignore
    let buildCommand (dbCommand : DbCommand) =
        let builder = StringBuilder()
        for commandIndex, command in commands |> Seq.indexed do
            addCommand builder dbCommand commandIndex command
        dbCommand.CommandText <- builder.ToString()

    member __.Evaluate() =
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
                        let! hasRow = reader.ReadAsync() : bool Task
                        if hasRow then
                            processor.ProcessRow()
                        else
                            hasRows <- false
                    resultSetCount <- resultSetCount + 1
                    let! hasNextResult = reader.NextResultAsync() : bool Task
                    match cmd.ResultSetCount with
                    | None -> // check for terminator
                        if not hasNextResult || reader.FieldCount = 1 && reader.GetName(0) = terminatorColumn i then
                            resultSetCount <- -1
                        else
                            let! hasNextResult = reader.NextResultAsync() : bool Task
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
    let evaluation = lazy builder.Evaluate()
    member __.Batch(cmd : #Command<'a>) =
        let index = builder.BatchCommand(cmd)
        fun () ->
            evaluation.Value.ContinueWith
                ( (fun (t : _ ResizeArray Task) -> t.Result.[index] |> Unchecked.unbox : 'a)
                , TaskContinuationOptions.ExecuteSynchronously
                )
