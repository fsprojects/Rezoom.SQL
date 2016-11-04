namespace Rezoom.SQL.Test
open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Rezoom.SQL
open Rezoom.SQL.Mapping

[<TestClass>]
type TestTypeErrors() =
    let expectError (msg : string) (sql : string) =
        let userModel = UserModel.Load("../../user-model-1", ".")
        try
            ignore <| CommandEffect.OfSQL(userModel.Model, "anonymous", sql)
            failwith "Should've thrown an exception!"
        with
        | :? SourceException as exn ->
            Assert.AreEqual(msg, exn.Reason)

    [<TestMethod>]
    member __.TestDifferentTypesComparedForEquality() =
        expectError "The types INT and STRING cannot be unified"
            """
                select g.*, u.*
                from Users u
                left join UserGroupMaps gm on gm.UserId = u.Id
                left join Groups g on g.Id = 'a'
                where g.Name like '%grp%' escape '%'
            """