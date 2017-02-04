module Rezoom.SQL.Test.TestAggregateErrors
open System
open NUnit.Framework
open FsUnit
open Rezoom.SQL
open Rezoom.SQL.Mapping

[<Test>]
let ``aggregates without group must not be found with non-aggregates`` () =
    expectError Error.columnNotAggregated
        """
            select sum(Id) as Sum, Id from Users
        """

[<Test>]
let ``aggregates with group by must not contain non-grouped column references`` () =
    expectError Error.columnNotGroupedBy
        """
            select Id, Name
            from Users
            group by Id
        """

[<Test>]
[<Ignore("fails right now, need an error for this")>]
let ``aggregates may not appear in where clause`` () =
    expectError ""
        """
            select count(*) as x from Users
            where count(*) > 0
        """