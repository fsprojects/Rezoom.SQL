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
    let results = TestDateTimeParameter.Command(DateTime.UtcNow)
    printfn "%A" results

type TestOptionalDateTimeParameter = SQL<"""
select * from Users where Created > @created or @created is null
""">

[<Test>]
let ``test optional datetime parameter`` () =
    let results = TestOptionalDateTimeParameter.Command(Some DateTime.UtcNow)
    printfn "%A" results

type TestGuidParameter = SQL<"""
select * from Users where RandomId = @id
""">

[<Test>]
let ``test guid parameter`` () =
    let results = TestGuidParameter.Command(Guid.NewGuid())
    printfn "%A" results

type TestOptionalGuidParameter = SQL<"""
select * from Users where RandomId = @id or @id is null
""">

[<Test>]
let ``test optional guid parameter`` () =
    let results = TestOptionalGuidParameter.Command(Some (Guid.NewGuid()))
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

