module TypeProviderUser.SQLite.TestUserPrimitiveEnum
open NUnit.Framework
open System
open Rezoom.SQL
open TypeProviderUser.UserTypes

// End-to-end coverage for mapping CLR enums via the user-type pipeline.
// Two flavors:
//   * FavoriteColor — F# enum (System.Enum subtype) mapped to string via
//     ToString / Enum.Parse. Underlying storage is human-readable.
//   * DateTimeKind — BCL enum mapped to int via the cast operator.
//     Underlying storage is the raw enum integer value.
//
// Each flavor gets the same three-test shape used for FileHash in
// TestUserPrimitiveByteArray: roundtrip, parameter equality, and optional
// parameter equality. Together that exercises the parameter-binding path,
// the result-set materialization path, and the option-wrapping path.

// --- FavoriteColor (ToString / Enum.Parse, string-underlying) ------------

type InsertAndSelectColors = SQL<"""
insert into ColorRows(Color) values(@c1);
insert into ColorRows(Color) values(@c2);
select Color from ColorRows order by Id;
""">

[<Test>]
let ``select roundtrips FavoriteColor values via ToString/Enum.Parse mapping`` () =
    let results =
        InsertAndSelectColors.Command(FavoriteColor.Red, FavoriteColor.Blue)
        |> runOnTestData
    Assert.AreEqual(2, results.Count)
    Assert.AreEqual(FavoriteColor.Red, results.[0].Color)
    Assert.AreEqual(FavoriteColor.Blue, results.[1].Color)

type FindColorByParameter = SQL<"""
insert into ColorRows(Color) values(@seed1);
insert into ColorRows(Color) values(@seed2);
select Color from ColorRows where Color = @needle;
""">

[<Test>]
let ``select with FavoriteColor parameter equality returns the matching row only`` () =
    let target = FavoriteColor.Red
    let other = FavoriteColor.Blue
    // Rezoom orders Command args alphabetically by name: needle, seed1, seed2.
    let results = FindColorByParameter.Command(target, other, target) |> runOnTestData
    Assert.AreEqual(1, results.Count)
    Assert.AreEqual(target, results.[0].Color)

type FindColorByOptionalParameter = SQL<"""
insert into ColorRows(Color) values(@seed1);
insert into ColorRows(Color) values(@seed2);
select Color from ColorRows where Color = @needle or @needle is null;
""">

[<Test>]
let ``select with optional FavoriteColor parameter filters when Some and returns all when None`` () =
    let target = FavoriteColor.Red
    let other = FavoriteColor.Blue
    // Command args alphabetical: needle, seed1, seed2.
    let withSome =
        FindColorByOptionalParameter.Command(Some target, target, other)
        |> runOnTestData
    Assert.AreEqual(1, withSome.Count)
    Assert.AreEqual(target, withSome.[0].Color)
    let withNone =
        FindColorByOptionalParameter.Command(None, target, other)
        |> runOnTestData
    Assert.AreEqual(2, withNone.Count)

// --- DateTimeKind (int unwrap, int-underlying) ---------------------------

type InsertAndSelectKinds = SQL<"""
insert into KindRows(Kind) values(@k1);
insert into KindRows(Kind) values(@k2);
insert into KindRows(Kind) values(@k3);
select Kind from KindRows order by Id;
""">

[<Test>]
let ``select roundtrips DateTimeKind values via raw int mapping`` () =
    let results =
        InsertAndSelectKinds.Command
            (DateTimeKind.Utc, DateTimeKind.Local, DateTimeKind.Unspecified)
        |> runOnTestData
    Assert.AreEqual(3, results.Count)
    Assert.AreEqual(DateTimeKind.Utc, results.[0].Kind)
    Assert.AreEqual(DateTimeKind.Local, results.[1].Kind)
    Assert.AreEqual(DateTimeKind.Unspecified, results.[2].Kind)

type FindKindByParameter = SQL<"""
insert into KindRows(Kind) values(@seed1);
insert into KindRows(Kind) values(@seed2);
select Kind from KindRows where Kind = @needle;
""">

[<Test>]
let ``select with DateTimeKind parameter equality returns the matching row only`` () =
    let target = DateTimeKind.Utc
    let other = DateTimeKind.Local
    // Command args alphabetical: needle, seed1, seed2.
    let results = FindKindByParameter.Command(target, other, target) |> runOnTestData
    Assert.AreEqual(1, results.Count)
    Assert.AreEqual(target, results.[0].Kind)

type FindKindByOptionalParameter = SQL<"""
insert into KindRows(Kind) values(@seed1);
insert into KindRows(Kind) values(@seed2);
select Kind from KindRows where Kind = @needle or @needle is null;
""">

[<Test>]
let ``select with optional DateTimeKind parameter filters when Some and returns all when None`` () =
    let target = DateTimeKind.Utc
    let other = DateTimeKind.Local
    // Command args alphabetical: needle, seed1, seed2.
    let withSome =
        FindKindByOptionalParameter.Command(Some target, target, other)
        |> runOnTestData
    Assert.AreEqual(1, withSome.Count)
    Assert.AreEqual(target, withSome.[0].Kind)
    let withNone =
        FindKindByOptionalParameter.Command(None, target, other)
        |> runOnTestData
    Assert.AreEqual(2, withNone.Count)
