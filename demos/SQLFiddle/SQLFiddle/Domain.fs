module SQLFiddle.Domain
open System.Collections.Generic
open Rezoom
open Rezoom.SQL.Compiler

let saveFiddle fiddleData = Persistence.saveFiddle fiddleData

let private sqliteBackend = SQLite.SQLiteBackend() :> IBackend
let private tsqlBackend = TSQL.TSQLBackend() :> IBackend
let private postgresBackend = Postgres.PostgresBackend() :> IBackend

let private backendOf fiddleBackend =
    match fiddleBackend with
    | SQLiteFiddle -> sqliteBackend
    | TSQLFiddle -> tsqlBackend
    | PostgresFiddle -> postgresBackend

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
        {   FiddleType.Nullable = columnType.Nullable
            FiddleType.Name = columnType.Type.ToString()
        }

let private parameterFrom (NamedParameter bindParam, columnType : ColumnType) =
    {   FiddleTypedName.Name = bindParam.Value
        FiddleTypedName.Type = typeFrom columnType
    }

let private resultSetFrom (queryInfo : QueryExprInfo<ColumnType>) =
    {   FiddleResultSet.Columns = queryInfo.Columns |> Seq.map (fun c ->
            {   FiddleTypedName.Name = c.ColumnName.Value
                FiddleTypedName.Type = typeFrom c.Expr.Info.Type
            }) |> Seq.toList
    }

let dispenserParameterIndexer() =
    let dict = Dictionary()
    let mutable last = -1
    { new IParameterIndexer with
        member __.ParameterIndex(par) =
            let succ, value = dict.TryGetValue(par)
            if succ then value
            else
                last <- last + 1
                dict.[par] <- last
                last
    }

let private typeInfoFrom (backend : IBackend) (model : CommandEffect) (effect : CommandEffect) =
    let indexer = dispenserParameterIndexer()
    let stringize statements =
        Rezoom.SQL.Mapping.CommandFragment.Stringize("\n", "    ", backend.ToCommandFragments(indexer, statements))
    {   FiddleTypeInformation.Parameters = effect.Parameters |> Seq.map parameterFrom |> Seq.toList
        ResultSets = effect.ResultSets() |> Seq.map resultSetFrom |> Seq.toList
        Idempotent =
            match effect.CacheInfo.Value with
            | None -> false
            | Some c -> c.Idempotent
        ReadTables =
            match effect.CacheInfo.Value with
            | None -> ["(unknown)"]
            | Some c -> [ for name in c.ReadTables -> name.ObjectName.Value ]
        WriteTables =
            match effect.CacheInfo.Value with
            | None -> ["(unknown)"]
            | Some c -> [ for name in c.WriteTables -> name.ObjectName.Value ]
        BackendModel = stringize model.Statements
        BackendCommand = stringize effect.Statements      
    }

let private validate (input : FiddleInput) =
    let backend = backendOf input.Backend
    let initialModel = backend.InitialModel
    try
        let modelEffect = CommandEffect.OfSQL(initialModel, "Model", input.Model)
        let model =
            defaultArg modelEffect.ModelChange initialModel
        try
            let commandEffect = CommandEffect.OfSQL(model, "Command", input.Command)
            FiddleValid (typeInfoFrom backend modelEffect commandEffect)
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