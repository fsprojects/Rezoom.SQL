module Rezoom.SQL.Test.TestTypeErrors
open System
open NUnit.Framework
open FsUnit
open Rezoom.SQL
open Rezoom.SQL.Mapping

[<Test>]
let ``incompatible types can't be compared for equality`` () =
    expectError "The types INT and STRING cannot be unified"
        """
            select g.*, u.*
            from Users u
            left join UserGroupMaps gm on gm.UserId = u.Id
            left join Groups g on g.Id = 'a'
            where g.Name like '%grp%' escape '%'
        """

[<Test>]
let ``unioned queries must have the same number of columns`` () =
    expectError "Expected 3 columns but selected 2"
        """
            select 1 a, 2 b, 3 c
            union all
            select 4, 5
        """

[<Test>]
let ``updates must set actual columns`` () =
    expectError "No such column to set: ``Nane``"
        """
            update Users
            set Id = 1, Nane = ''
            where Id > 5
        """

[<Test>]
let ``updated column types must match`` () =
    expectError "The types INT and STRING cannot be unified"
        """
            update Users
            set Id = 'five'
        """

[<Test>]
let ``inserted column types must match`` () =
    expectError "The types STRING and INT cannot be unified"
        """
            insert into Users(Id, Name) values ('one', 'jim')
        """

[<Test>]
let ``inserted columns must exist`` () =
    expectError "No such column: ``Goober``"
        """
            insert into Users(Goober, Booger) values ('one', 'jim')
        """

[<Test>]
let ``sum argument must be numeric`` () =
    expectError "The type STRING is not one of (INT64 | FLOAT64 | DECIMAL)"
        """
            select sum(Name) from Users
        """

[<Test>]
let ``aggregates without group must not be found with non-aggregates`` () =
    let msg =
        "Can't reference column outside of an aggregate function "
        + "because this query uses aggregate functions without a GROUP BY clause"
    expectError msg
        """
            select sum(Id) as Sum, Id from Users
        """

[<Test>]
let ``aggregates with group by must not contain non-grouped column references`` () =
    let msg =
        "Can't reference column outside of an aggregate function "
        + "because the GROUP BY clause does not include this column"
    expectError msg
        """
            select Id, Name
            from Users
            group by Id
        """