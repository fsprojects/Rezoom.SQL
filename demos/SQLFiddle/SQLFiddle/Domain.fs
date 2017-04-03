module SQLFiddle.Domain
open Rezoom
open Rezoom.SQL.Compiler

let saveFiddle fiddleData = Persistence.saveFiddle fiddleData

let private sqliteBackend = SQLite.SQLiteBackend() :> IBackend
let private tsqlBackend = TSQL.TSQLBackend() :> IBackend

let private backendOf fiddleBackend =
    match fiddleBackend with
    | SQLiteFiddle -> sqliteBackend
    | TSQLFiddle -> tsqlBackend

let private errorFrom ty (exn : SQLCompilerException) =
    let src, reason, message =
        match exn with
        | :? SourceException as src ->
            src.SourceInfo, src.Reason, src.Message
        | :? SourceInfoException as src ->
            src.SourceInfo, src.Message, src.Message
        | _ ->
            SourceInfo.Invalid, exn.Message, exn.Message
    {   Type = ty
        StartLine = src.StartPosition.Line
        StartColumn = src.StartPosition.Column
        EndLine = src.EndPosition.Line
        EndColumn = src.EndPosition.Column
        Reason = reason
        Message = message
    }

let private typeFrom (columnType : ColumnType) =
        {   Nullable = columnType.Nullable
            Name = columnType.Type.ToString()
        }

let private parameterFrom (NamedParameter bindParam, columnType : ColumnType) =
    {   Name = bindParam.Value
        Type = typeFrom columnType
    }

let private resultSetFrom (queryInfo : QueryExprInfo<ColumnType>) =
    {   Columns = queryInfo.Columns |> Seq.map (fun c ->
            {   Name = c.ColumnName.Value
                Type = typeFrom c.Expr.Info.Type
            }) |> Seq.toList
    }

let private typeInfoFrom (effect : CommandEffect) =
    {   Parameters = effect.Parameters |> Seq.map parameterFrom |> Seq.toList
        ResultSets = effect.ResultSets() |> Seq.map resultSetFrom |> Seq.toList
    }

let private validate (input : FiddleInput) =
    let backend = backendOf input.Backend
    let initialModel = backend.InitialModel
    try
        let model =
            let modelEffect = CommandEffect.OfSQL(initialModel, "Model", input.Model)
            defaultArg modelEffect.ModelChange initialModel
        try
            let commandEffect = CommandEffect.OfSQL(model, "Command", input.Command)
            FiddleValid (typeInfoFrom commandEffect)
        with
        | :? SQLCompilerException as exn ->
            FiddleInvalid (errorFrom CommandError exn)
    with
    | :? SQLCompilerException as exn ->
        FiddleInvalid (errorFrom ModelError exn)

let getFiddle id =
    plan {
        let! input = Persistence.getFiddle id
        let output = validate input
        return { Input = input; Output = output }
    }

let checkFiddle input =
    plan {
        let output = validate input
        return { Input = input; Output = output }
    }