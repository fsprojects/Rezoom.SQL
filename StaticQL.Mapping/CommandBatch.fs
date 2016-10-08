namespace StaticQL.Mapping
open System
open System.Data
open System.Data.Common
open System.Collections.Generic
open System.Text

type CommandBatch(conn : DbConnection) =
    static let terminatorColumn i = "STATICQL_TERMINATOR_" + string i
    static let terminator i = ";--'*/;SELECT NULL AS " + terminatorColumn i
    static let parameterName i = "@STATICQL_" + string i
    static let localName i name = "STATICQL_" + name + "_" + string i
    let commands = ResizeArray<Command>()

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

    let evaluate() = async {
        use dbCommand = conn.CreateCommand()
        buildCommand dbCommand
        use! reader = Async.AwaitTask <| dbCommand.ExecuteReaderAsync()
        let processed = ResizeArray()
        for i = 0 to commands.Count - 1 do
            let cmd = commands.[i]
            let mutable resultSetCount = match cmd.ResultSetCount with | Some 0 -> -1 | _ -> 0
            while resultSetCount >= 0 do
                cmd.BeginResultSet(reader)
                let mutable hasRows = true
                while hasRows do
                    let! hasRow = Async.AwaitTask <| reader.ReadAsync()
                    if hasRow then
                        cmd.ProcessRow()
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
            processed.Add(cmd.GetResultObject())
        return processed
    }

    let evaluation = lazy evaluate()

    member private this.Batch(cmd : Command) =
        let index = commands.Count
        commands.Add(cmd)
        fun () ->
            async {
                let! allCommandResults = evaluation.Value
                return allCommandResults.[index]
            }
    member this.Batch(cmd : #Command<'a>) =
        let batched = this.Batch(cmd :> Command)
        fun () ->
            async {
                let! boxed = batched()
                return (Unchecked.unbox boxed : 'a)
            }
