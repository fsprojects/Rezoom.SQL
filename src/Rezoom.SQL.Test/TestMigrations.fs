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