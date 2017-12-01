module TypeProviderUser.SQLite.TestSelects
open System
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open FsUnit
open Rezoom
open Rezoom.SQL
open Rezoom.SQL.Plans
open MBrace.FsPickler
open Rezoom

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


type TestInEmptySet = SQL<"""
select * from Users where RandomId in @ids
""">

[<Test>]
let ``test in empty set`` () =
    let results = TestInEmptySet.Command([||]) |> runOnTestData
    Assert.AreEqual(0, results.Count)
    printfn "%A" results

[<Test>]
let ``test optional guid parameter`` () =
    let results = TestOptionalGuidParameter.Command(Some (Guid.NewGuid())) |> runOnTestData
    printfn "%A" results

type TestEmptyMany = SQL<"""
select p.*, many Children(c.*)
from Users p
left join Users c on false
""">

[<Test>]
let ``test empty many`` () =
    let results = TestEmptyMany.Command() |> runOnTestData
    Assert.AreEqual(2, results.Count)
    for result in results do
        Assert.AreEqual(0, result.Children.Count)
    printfn "%A" results

[<Test>]
let ``replay works`` () =
    let plan =
        plan {
            let! r1 = TestInInteger.Command([| 1L |]).Plan()
            let! r2 = TestInInteger.Command([| 2L |]).Plan()
            return r1.[0].Email, r2.[0].Email
        }
    let config = Execution.ExecutionConfig.Default
    let serializer =
        let bin = FsPickler.CreateBinarySerializer()
        { new Replay.IReplaySerializer with
            member __.Serialize(o) = bin.Pickle(o)
            member __.Deserialize(o) = bin.UnPickle(o)
        }
    let mutable saved = None
    let save state arr =
        saved <- Some (arr())
    let recording =
        Replay.RecordingExecutionStrategy.Create
            ( Execution.defaultExecutionStrategy
            , serializer
            , save
            )
    let played = recording.Execute(config, plan, CancellationToken.None).Result
    match saved with
    | None -> failwith "not saved"
    | Some blob ->
        let replayed = (Replay.replay config serializer blob).Result
        if played = unbox replayed then
            ()
        else failwith "not equal"

type InsertPicture = SQL<"insert into Pictures row SHA256 = @sha, PNGData = @png">

[<Test>]
let ``lotsa parameters`` () =
    let task =
        plan {
            let g() = Guid.NewGuid().ToByteArray()
            for i in batch [0..2000] do
                do! InsertPicture.Command(g(), g()).Plan()
        } |> Execution.execute Execution.ExecutionConfig.Default
    task.Wait()

open Rezoom.SQL.Raw
open System.Data

type RawSQLQuery = SQL<"""
    select * from Users where unsafe_inject_raw(@whereClause)
""">

[<Test>]
let ``test raw sql parameter`` () =
    let results =
        RawSQLQuery.Command(whereClause = [| sql "1="; arg 1 |]) |> runOnTestData
    for result in results do
        printfn "%A" result.Email


