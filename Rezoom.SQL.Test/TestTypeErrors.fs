module Rezoom.SQL.Test.TestTypeErrors
open System
open NUnit.Framework
open FsUnit
open Rezoom.SQL
open Rezoom.SQL.Mapping

let expectError (msg : string) (sql : string) =
    let userModel = userModel1()
    try
        ignore <| CommandEffect.OfSQL(userModel.Model, "anonymous", sql)
        failwith "Should've thrown an exception!"
    with
    | :? SourceException as exn ->
        printfn "\"%s\"" exn.Message
        Assert.AreEqual(msg, exn.Reason)

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