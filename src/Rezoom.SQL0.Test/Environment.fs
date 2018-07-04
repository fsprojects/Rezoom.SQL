[<AutoOpen>]
module Rezoom.SQL.Test.Environment
open NUnit.Framework
open FsUnit
open System
open System.Reflection
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions
open Rezoom.SQL.Mapping
open Rezoom.SQL.Compiler

let userModelByName name =
    let assemblyFolder = Path.GetDirectoryName(Uri(Assembly.GetExecutingAssembly().CodeBase).LocalPath)
    let resolutionFolder = Path.Combine(assemblyFolder, "../../" + name)
    UserModel.Load(resolutionFolder, ".")

let userModel1() = userModelByName "user-model-1"

let userModel2() = userModelByName "user-model-2"

let expectError (msg : string) (sql : string) =
    let userModel = userModel1()
    try
        ignore <| CommandEffect.OfSQL(userModel.Model, "anonymous", sql)
        failwith "Should've thrown an exception!"
    with
    | :? SourceException as exn ->
        printfn "\"%s\"" exn.Message
        Assert.AreEqual(msg, exn.Reason.Trim())

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

let dumpSignature (fty : FunctionType) =
    sprintf "## %O\n    %O%O\n| Idempotent | Erased | Aggregate |\n|-|-|-|\n|%s|%s|%s|"
        fty.FunctionName
        fty.FunctionName
        fty.TypeSignature
        (if fty.Idempotent then "yes" else "no")
        (if fty.Erased then "yes" else "no")
        (match fty.Aggregate ArgumentWildcard with
            | None -> "no"
            | Some agg -> "yes")


type SimpleTestCheck =
    {   Idempotent : bool option
        ResultSets : (string * ColumnType) list list option
        Parameters : (string * ColumnType) list option
        OutputMigration : string option
        OutputCommand : string option
        WriteTables : string list option
        ReadTables : string list option
    }

let expect =
    {   Idempotent = None
        ResultSets = None
        Parameters = None
        OutputMigration = None
        OutputCommand = None
        WriteTables = None
        ReadTables = None
    }

type SimpleTestExpectation =
    | Good of SimpleTestCheck
    | BadCommand of string
    | BadMigration of string

type SimpleTest =
    {   Migration : string
        Command : string
        TestBackend : IBackend
        Expect : SimpleTestExpectation
    }

let defaultTest =
    {   Migration = ""
        Command = ""
        TestBackend = DefaultBackend()
        Expect = BadCommand "expectation not specified"
    }

let sqliteTest =
    { defaultTest with
        TestBackend = SQLite.SQLiteBackend()
    }

let tsqlTest =
    { defaultTest with
        TestBackend = TSQL.TSQLBackend()
    }

let postgresTest =
    { defaultTest with
        TestBackend = Postgres.PostgresBackend()
    }

let private runSimple (test : SimpleTest) =
    let indexer = dispenserParameterIndexer()
    try
        let migrationEffect = CommandEffect.OfSQL(test.TestBackend.InitialModel, "migration", test.Migration)
        let outputMigration =
            test.TestBackend.ToCommandFragments(indexer, migrationEffect.Statements)
            |> CommandFragment.Stringize
        let commandModel = migrationEffect.ModelChange |? test.TestBackend.InitialModel
        try
            let commandEffect = CommandEffect.OfSQL(commandModel, "command", test.Command)
            let outputCommand =
                test.TestBackend.ToCommandFragments(indexer, commandEffect.Statements)
                |> CommandFragment.Stringize
            {   Idempotent = match commandEffect.CacheInfo.Value with | Some v -> Some v.Idempotent | None -> None
                ResultSets =
                    [ for resultSet in commandEffect.ResultSets() ->
                        [ for col in resultSet.Columns ->
                            col.ColumnName.Value, col.Expr.Info.Type
                        ]
                    ] |> Some
                Parameters =
                    [ for (NamedParameter par), ty in commandEffect.Parameters ->
                        par.Value, ty
                    ] |> Some
                OutputMigration = Some outputMigration
                OutputCommand = Some outputCommand
                WriteTables =
                    match commandEffect.CacheInfo.Value with
                    | None -> None
                    | Some x -> x.WriteTables |> Seq.map string |> Seq.toList |> Some
                ReadTables =
                    match commandEffect.CacheInfo.Value with
                    | None -> None
                    | Some x -> x.ReadTables |> Seq.map string |> Seq.toList |> Some
            } |> Good
        with
        | :? SourceException as sexn ->
            BadCommand sexn.Reason
        | :? SQLCompilerException as cexn ->
            BadCommand cexn.Message
    with
    | :? SourceException as sexn ->
        BadMigration sexn.Reason
    | :? SQLCompilerException as mexn ->
        BadMigration mexn.Message

type String with
    member this.SmushWhitespace() =
        Regex.Replace(this.Trim(), @"[ \r\n]+", " ")

let private assertMatchExpectation (expect : SimpleTestExpectation) (result : SimpleTestExpectation) =
    let (?==) l r =
        match l, r with
        | None, _ -> true
        | Some _ as x, y when x = y -> true
        | _ -> false
            
    match expect, result with
    | BadCommand e, BadCommand r
    | BadMigration e, BadMigration r when e = r -> ()
    | Good e, Good r ->
        let smush (s : string option) = s |> Option.map (fun s -> s.SmushWhitespace())
        let matched =
            e.Idempotent ?== r.Idempotent
            && e.ResultSets ?== r.ResultSets
            && e.Parameters ?== r.Parameters
            && smush e.OutputCommand ?== smush r.OutputCommand
            && smush e.OutputMigration ?== smush r.OutputMigration
            && e.WriteTables ?== r.WriteTables
            && e.ReadTables ?== r.ReadTables
        if matched then () else failwithf "Mismatch: %A vs %A" e r
    | e, r ->
        failwithf "Mismatch: %A vs %A" e r

let assertSimple (test : SimpleTest) =
    let ran = runSimple test
    assertMatchExpectation test.Expect ran
    