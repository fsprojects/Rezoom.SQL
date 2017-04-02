module TypeProviderUser.SQLite.TestPickles
open System.Collections.Generic
open NUnit.Framework
open FsUnit
open Rezoom.SQL
open MBrace.FsPickler

type GetAllUsers = SQL<"""
select * from Users
""">

[<Test>]
let ``test pickler serialization`` () =
    let results = GetAllUsers.Command() |> runOnTestData
    printfn "%A" results

    let binarySerializer = FsPickler.CreateBinarySerializer()

    let pickle = binarySerializer.Pickle results
    let unpickled = binarySerializer.UnPickle<GetAllUsers.Row IReadOnlyList> pickle

    printfn "%A" unpickled
    Assert.AreEqual(results.Count, unpickled.Count)
    Assert.IsTrue((results, unpickled) ||> Seq.forall2 (fun left right ->
        left.Id = right.Id
        && left.Email = right.Email
        && left.Name = right.Name
        && left.ProfilePictureSHA256 = right.ProfilePictureSHA256))
