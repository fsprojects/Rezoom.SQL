module Rezoom.SQL.Test.TestMigrations
open System
open System.IO
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Compiler
open Rezoom.SQL.Mapping
open Rezoom.SQL.Migrations

type Tree =
    {   MigrationName : string
        Children : Tree list
    }

let private branch migrationName children =
    {   MigrationName = migrationName
        Children = children
    }

let private leaf migrationName = branch migrationName []

let rec toTree (migraton : string MigrationTree) =
    {   MigrationName = migraton.Node.MigrationName
        Children = toTrees migraton.Children
    }
and toTrees migrations = migrations |> Seq.map toTree |> Seq.toList

[<Test>]
let ``model 2 migration tree`` () =
    let model = userModel2()
    let migrations = model.Migrations
    let expected =
        [   branch "V1, initial"
                [   leaf "V1, employer"
                    leaf "V1, groups"
                ]
            leaf "V2, companygroups"
        ]
    printfn "%A" migrations
    let trees = toTrees migrations
    printfn "%A" trees
    Assert.AreEqual(expected, trees)

[<Test>]
let ``model 3 fails`` () =
    try
        ignore <| userModelByName "user-model-3"
        failwith "should've failed"
    with
    | :? SQLCompilerException as c when c.Message = Error.minorMigrationContainsDestruction "V1, drop" ->
        ()
    | other ->
        printfn "wrong exn %O" other
        reraise()

[<Test>]
let ``model 4 fails`` () =
    try
        ignore <| userModelByName "user-model-4"
        failwith "should've failed"
    with
    | :? SQLCompilerException as c when c.Message = Error.migrationContainsParameter "V2, bad" ->
        ()
    | other ->
        printfn "wrong exn %O" other
        reraise()

[<Test>]
let ``model 5 migration tree`` () =
    let model = userModelByName "user-model-5"
    let migrations = model.Migrations
    let expected =
        [   branch "V1, model"
                [   branch "V1, comments"
                        [   leaf "V1, time"
                        ]
                    branch "V1, groups"
                        [   branch "V1, foos"
                                [   leaf "V1, bars"
                                ]
                        ]
                ]
            branch "V2, next"
                [   leaf "V2, baz"
                    leaf "V2, qux"
                ]
        ]
    printfn "%A" migrations
    let trees = toTrees migrations
    printfn "%A" trees
    Assert.AreEqual(expected, trees)

[<Test>]
let ``model 6 fails`` () =
    try
        ignore <| userModelByName "user-model-6"
        failwith "should've failed"
    with
    | :? SQLCompilerException as c when c.Message = Error.noSuchTable "Foos" ->
        ()
    | other ->
        printfn "wrong exn %O" other
        reraise()

[<Test>]
let ``model 6-good migration tree`` () =
    let model = userModelByName "user-model-6-good"
    let migrations = model.Migrations
    let expected =
        [   branch "V1, model"
                [   branch "V1, a"
                        [   leaf "V1, b"
                        ]
                ]
        ]
    printfn "%A" migrations
    let trees = toTrees migrations
    printfn "%A" trees
    Assert.AreEqual(expected, trees)