module Rezoom.SQL.Test.TestRowTypesDeclarations
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Compiler

[<Test>]
let ``simple select with rowtypes`` () =
    let userModel = userModel1()
    let parsed = CommandEffect.OfSQL(userModel.Model, "anonymous", "select<ABC, System.DEF> * from (select 1 x, 2 y) sq")
    let sets = parsed.ResultSets() |> Seq.toArray
    Assert.AreEqual(1, sets.Length)
    let set = sets.[0]
    Assert.AreEqual(Some [ "ABC"; "System.DEF" ], set.RowTypes |> Option.map Seq.toList)

[<Test>]
let ``compound expr with rowtypes`` () =
    let userModel = userModel1()
    let parsed = CommandEffect.OfSQL(userModel.Model, "anonymous",
        "select<Foo> * from (select 1 x, 2 y) sq union all select 3, 4 intersect select 5, 6")
    let sets = parsed.ResultSets() |> Seq.toArray
    Assert.AreEqual(1, sets.Length)
    let set = sets.[0]
    Assert.AreEqual(Some [ "Foo" ], set.RowTypes |> Option.map Seq.toList)

[<Test>]
let ``rowtypes with spaces`` () =
    let userModel = userModel1()
    let parsed = CommandEffect.OfSQL(userModel.Model, "anonymous",
        "select < Foo, Bar > * from (select 1 x, 2 y) sq union all select 3, 4 intersect select 5, 6")
    let sets = parsed.ResultSets() |> Seq.toArray
    Assert.AreEqual(1, sets.Length)
    let set = sets.[0]
    Assert.AreEqual(Some [ "Foo"; "Bar" ], set.RowTypes |> Option.map Seq.toList)
