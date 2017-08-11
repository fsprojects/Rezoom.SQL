module TypeProviderUser.SQLite.TestSelects
open System
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open FsUnit
open Rezoom
open Rezoom.SQL
open Rezoom.SQL.Plans

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

type TestDateTimeParameter = SQL<"""
select * from Users where Created > @created
""">

[<Test>]
let ``test datetime parameter`` () =
    let results = TestDateTimeParameter.Command(DateTime.UtcNow) |> runOnTestData
    printfn "%A" results

type TestOptionalDateTimeParameter = SQL<"""
select * from Users where Created > @created or @created is null
""">

[<Test>]
let ``test optional datetime parameter`` () =
    let results = TestOptionalDateTimeParameter.Command(Some DateTime.UtcNow) |> runOnTestData
    printfn "%A" results

type TestGuidParameter = SQL<"""
create temp table bar(name string);
select * from Users where RandomId = @id;
drop table temp.bar;
""">

[<Test>]
let ``test guid parameter`` () =
    let results = TestGuidParameter.Command(Guid.NewGuid()) |> runOnTestData
    printfn "%A" results

type TestOptionalGuidParameter = SQL<"""
select * from Users where RandomId = @id or @id is null
""">

[<Test>]
let ``test optional guid parameter`` () =
    let results = TestOptionalGuidParameter.Command(Some (Guid.NewGuid())) |> runOnTestData
    printfn "%A" results


type TestInEmptySet = SQL<"""
select * from Users where RandomId in @ids
""">

[<Test>]
let ``test in empty set`` () =
    let results = TestInEmptySet.Command([||]) |> runOnTestData
    Assert.AreEqual(0, results.Count)
    printfn "%A" results


