module Rezoom.SQL.Test.TestModel
open NUnit.Framework
open FsUnit
open Rezoom.SQL

[<Test>]
let ``model 2 loads`` () =
    let model = userModel2()
    let schema = model.Model.Schemas.[model.Model.DefaultSchema]
    Assert.AreEqual(15, schema.Objects.Count)