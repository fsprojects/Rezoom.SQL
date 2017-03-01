module Rezoom.SQL.Test.TestStaticRowCount
open System
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Compiler
open Rezoom.SQL.Mapping

let private resultSets expected sql =
    let userModel = userModel1()
    let effect = CommandEffect.OfSQL(userModel.Model, "anonymous", sql)
    let resultSet = effect.ResultSets() |> Seq.exactlyOne
    Assert.AreEqual(expected, resultSet.StaticRowCount)

[<Test>]
let ``single select 1`` () =
    resultSets (Some 1)
        """
            select 1 as x;
        """

[<Test>]
let ``single select 2 union alled`` () =
    resultSets (Some 2)
        """
            select 1 as x
            union all
            select 2;
        """

[<Test>]
let ``single select 1 unioned`` () =
    resultSets None
        """
            select 1 as x
            union
            select 2;
        """

[<Test>]
let ``select from`` () =
    resultSets None
        """
            select 1 as x
            from Users
        """

[<Test>]
let ``select from unioned`` () =
    resultSets None
        """
            select 1 as x
            union all
            select 1 as x
            from Users;
        """

[<Test>]
let ``select from select`` () =
    resultSets None
        // even though this could be 1, we play it safe and say None, since there could be a where etc.
        // in the subquery
        """select 1 as x from (select 1 as y) q"""

[<Test>]
let ``select 1 with where`` () =
    resultSets None
        """select 1 as x where false"""

[<Test>]
let ``select 1 with limit`` () =
    resultSets None
        """select 1 as x limit 0"""