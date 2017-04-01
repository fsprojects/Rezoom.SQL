module TypeProviderUser.SQLite.TestSelects
open System
open NUnit.Framework
open FsUnit
open Rezoom.SQL

type TestEqualInteger = SQL<"""
select * from Users where Id = @userId
""">

[<Test>]
let ``test = integer`` () =
    let results = TestEqualInteger.Command(1L) |> runOnTestData
    printfn "%A" results
    Assert.AreEqual
        ( [ "Homer" ]
        , [ for r in results -> r.Name ] |> List.sort
        )

type TestInInteger = SQL<"""
select * from Users where Id in @userIds
""">

[<Test>]
let ``test in integer`` () =
    let results = TestInInteger.Command([| 1L; 2L |]) |> runOnTestData
    printfn "%A" results
    Assert.AreEqual
        ( [ "Homer"; "Marge" ]
        , [ for r in results -> r.Name ] |> List.sort
        )

type TestInByteArrays = SQL<"""
select * from Pictures where SHA256 in @hashes
""">

[<Test>]
let ``test in byte arrays`` () =
    let results = TestInByteArrays.Command([| Array.create 32 0uy; Array.create 32 0xffuy |]) |> runOnTestData
    printfn "%A" results
    Assert.AreEqual([ [||]; [||] ], [ for r in results -> r.PNGData ])