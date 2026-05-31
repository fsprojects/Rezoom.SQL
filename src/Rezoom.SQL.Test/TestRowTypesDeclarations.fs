module Rezoom.SQL.Test.TestRowTypesDeclarations
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Compiler
open Rezoom.SQL.Mapping
open Rezoom.SQL.Test.UserTypes

let userModel = lazy userModelByName "user-model-7-usertypes"

[<Test>]
let ``simple select with rowtypes`` () =
    let parsed = CommandEffect.OfSQL(userModel.Value.Model, "anonymous",
        "select<IRowXYZ, Rezoom.SQL.Test.UserTypes.IFoo> * from (select 1 x, 2 y) sq", userModel.Value.UserTypeLibrary)
    let sets = parsed.ResultSets() |> Seq.toArray
    Assert.AreEqual(1, sets.Length)
    let set = sets.[0]
    let expected = Some [ { UserCLRType = typeof<IRowXYZ> }; { UserCLRType = typeof<IFoo> } ]
    Assert.AreEqual(expected, set.RowTypes |> Option.map Seq.toList)

[<Test>]
let ``compound expr with rowtypes`` () =
    let parsed = CommandEffect.OfSQL(userModel.Value.Model, "anonymous",
        "select<IFoo> * from (select 1 x, 2 y) sq union all select 3, 4 intersect select 5, 6", userModel.Value.UserTypeLibrary)
    let sets = parsed.ResultSets() |> Seq.toArray
    Assert.AreEqual(1, sets.Length)
    let set = sets.[0]
    let expected = Some [ { UserCLRType = typeof<IFoo> } ]
    Assert.AreEqual(expected, set.RowTypes |> Option.map Seq.toList)

[<Test>]
let ``rowtypes with spaces`` () =
    let parsed = CommandEffect.OfSQL(userModel.Value.Model, "anonymous",
        "select < IFoo, IBar > * from (select 1 x, 2 y) sq union all select 3, 4 intersect select 5, 6", userModel.Value.UserTypeLibrary)
    let sets = parsed.ResultSets() |> Seq.toArray
    Assert.AreEqual(1, sets.Length)
    let set = sets.[0]
    let expected = Some [ { UserCLRType = typeof<IFoo> }; { UserCLRType = typeof<IBar> } ]
    Assert.AreEqual(expected, set.RowTypes |> Option.map Seq.toList)
