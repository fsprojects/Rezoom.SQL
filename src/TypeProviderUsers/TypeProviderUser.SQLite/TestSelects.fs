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
open TypeProviderUser.UserTypes

type TestEqualInteger = SQL<"""
select * from Users where Id = @userId
""">

[<Test>]
let ``test = integer`` () =
    let results = TestEqualInteger.Command(UserId 1L) |> runOnTestData
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
    let results = TestInInteger.Command([| UserId 1L; UserId 2L |]) |> runOnTestData
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

// Exercises the F# extension-method form of a custom user primitive
// (TypeProviderUser.UserTypes.Extensions extends System.TimeOnly with
// ToPrimitive/FromPrimitive). This is the BCL-type-extension path that
// requires SourceAssemblies to use the *declaring* assembly of the
// converter methods rather than the UserCLRType's assembly (otherwise
// MLC's System.Runtime would leak into ProvidedTypes' source→target
// mapping and break every System.String/DateTime/Guid splice site).
type SelectTime = SQL<"""
select Name, FavoriteTimeOfDay from Users where Name = 'Homer';
""">

[<Test>]
let ``test TimeOnly user primitive read`` () =
    let results = SelectTime.Command() |> runOnTestData
    Assert.AreEqual(1, results.Count)
    Assert.AreEqual("Homer", results.[0].Name)
    // We seeded FavoriteTimeOfDay with the default '00:00:00.0000000'.
    Assert.AreEqual(TimeOnly(0, 0, 0), results.[0].FavoriteTimeOfDay)

type UpdateTimeAndRead = SQL<"""
update Users set FavoriteTimeOfDay = @t where Name = 'Homer';
select Name, FavoriteTimeOfDay from Users where Name = 'Homer';
""">

[<Test>]
let ``test TimeOnly user primitive write+read roundtrip`` () =
    // Roundtrips the F#-extension-on-BCL-type ToPrimitive (TimeOnly -> string)
    // path through a parameter, and the FromPrimitive (string -> TimeOnly)
    // path through the SELECT.
    let t = TimeOnly(13, 37, 42)
    let results = UpdateTimeAndRead.Command(t) |> runOnTestData
    Assert.AreEqual(1, results.Count)
    Assert.AreEqual(t, results.[0].FavoriteTimeOfDay)

type SelectTimeOnlyScalar = SQL<"""
select FavoriteTimeOfDay from Users where Name = 'Homer';
""">

[<Test>]
let ``test TimeOnly user primitive single-column scalar`` () =
    let results = SelectTimeOnlyScalar.Command() |> runOnTestData
    Assert.AreEqual(1, results.Count)

// Single-column row whose CLR type is a user-DLL UserPrimitive (UserId is a
// single-case DU defined in TypeProviderUser.UserTypes, loaded into the
// design-time host via MetadataLoadContext). This exercises addScalarInterface
// in TypeGeneration.fs — passing an MLC-flavoured Type to MakeGenericType
// previously produced a TypeBuilderInstantiation and threw at the GetMethod
// call. Multi-column variants (e.g. `select Name, Id from Users`) sidestep
// the bug since IScalar is only added for single-column rows.
type SelectUserIdScalar = SQL<"""
select Id from Users where Name = 'Homer';
""">

[<Test>]
let ``test UserId user primitive single-column scalar`` () =
    let results = SelectUserIdScalar.Command() |> runOnTestData
    Assert.AreEqual(1, results.Count)
    Assert.AreEqual(UserId 1L, results.[0].Id)

type TimeOnlyListParam = SQL<"""
select * from Users where FavoriteTimeOfDay in @times;
""">

[<Test>]
let ``test TimeOnly user primitive list parameter`` () =
    // List-of-UserPrimitive parameter — exercises the per-element
    // ToPrimitive call inside the ListType ParameterTransform path in
    // TypeGeneration.fs.
    let results =
        TimeOnlyListParam.Command([| TimeOnly(0, 0, 0); TimeOnly(13, 37, 42) |])
        |> runOnTestData
    // Both seeded users have default time 00:00:00 — both should match.
    Assert.AreEqual(2, results.Count)

type OptionalTimeOnlyParam = SQL<"""
select * from Users where FavoriteTimeOfDay = @t or @t is null;
""">

[<Test>]
let ``test TimeOnly user primitive optional parameter`` () =
    // Option<TimeOnly> parameter exercises the optionalsToDbNull path in
    // Backend.fs combined with the user-primitive unwrapper.
    let results =
        OptionalTimeOnlyParam.Command(Some (TimeOnly(0, 0, 0)))
        |> runOnTestData
    Assert.AreEqual(2, results.Count)
    let results2 = OptionalTimeOnlyParam.Command(None) |> runOnTestData
    Assert.AreEqual(2, results2.Count)

type SetAndReadBedtime = SQL<"""
update Users set BedtimeIfAny = @bedtime where Name = 'Homer';
select Name, BedtimeIfAny from Users order by Name;
""">

[<Test>]
let ``test nullable TimeOnly column roundtrip with Some`` () =
    let t = TimeOnly(22, 30, 0)
    let results = SetAndReadBedtime.Command(Some t) |> runOnTestData
    Assert.AreEqual(2, results.Count)
    let homer = results |> Seq.find (fun r -> r.Name = "Homer")
    let marge = results |> Seq.find (fun r -> r.Name = "Marge")
    Assert.AreEqual(Some t, homer.BedtimeIfAny)
    Assert.AreEqual(None, marge.BedtimeIfAny)

[<Test>]
let ``test nullable TimeOnly column roundtrip with None`` () =
    let results = SetAndReadBedtime.Command(None) |> runOnTestData
    Assert.AreEqual(2, results.Count)
    for r in results do Assert.AreEqual(None, r.BedtimeIfAny)

type SetAndReadSignupDate = SQL<"""
update Users set SignupDate = @date where Name = 'Homer';
select Name, SignupDate from Users order by Name;
""">

[<Test>]
let ``test BCL-extension-only user-types DLL roundtrip`` () =
    // SignupDate is System.DateOnly, extended via ToPrimitive/FromPrimitive in
    // TypeProviderUser.BclOnlyTypes — a user-types DLL that contains ONLY a BCL
    // extension and NO single-case DU. The only way that DLL gets registered as a
    // ProvidedTypes source assembly is via the *declaring* assembly of its
    // To/FromPrimitive methods (UserTypeLibrary.SourceAssemblies). If that used
    // UserCLRType.Assembly instead, DateOnly's assembly would resolve to the MLC
    // System.Runtime (dropped), the DLL would never be registered, and DateOnly's
    // FromPrimitive would fail to resolve in the generated command.
    let d = System.DateOnly(2017, 1, 1)
    let results = SetAndReadSignupDate.Command(Some d) |> runOnTestData
    Assert.AreEqual(2, results.Count)
    let homer = results |> Seq.find (fun r -> r.Name = "Homer")
    let marge = results |> Seq.find (fun r -> r.Name = "Marge")
    Assert.AreEqual(Some d, homer.SignupDate)
    Assert.AreEqual(None, marge.SignupDate)

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
    // Seed the DB once via the sync helper, then exercise the plan-based path.
    TestInInteger.Command([| UserId 1L |]) |> runOnTestData |> ignore
    let plan =
        plan {
            let! r1 = TestInInteger.Command([| UserId 1L |]).Plan()
            let! r2 = TestInInteger.Command([| UserId 2L |]).Plan()
            return r1.[0].Email, r2.[0].Email
        }
    let config = executionConfig
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
    // Migrate + seed by running any test command first; runOnTestData drops/recreates
    // the SQLite file each invocation.
    TestInInteger.Command([| UserId 1L |]) |> runOnTestData |> ignore
    let task =
        plan {
            let g() = Guid.NewGuid().ToByteArray()
            for i in batch [0..2000] do
                do! InsertPicture.Command(g(), g()).Plan()
        } |> Execution.execute executionConfig
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


