module TypeProviderUser.SQLite.TestUserPrimitiveByteArray
open NUnit.Framework
open Rezoom.SQL
open TypeProviderUser.UserTypes

type InsertAndSelectFileHashes = SQL<"""
insert into HashedBlobs(Hash) values(@h1);
insert into HashedBlobs(Hash) values(@h2);
select Hash from HashedBlobs order by Id;
""">

[<Test>]
let ``select roundtrips a FileHash user primitive over byte[]`` () =
    let h1 = FileHash [| 0x01uy; 0x02uy; 0x03uy; 0x04uy |]
    let h2 = FileHash [| 0xFFuy; 0xEEuy; 0xDDuy; 0xCCuy |]
    let results = InsertAndSelectFileHashes.Command(h1, h2) |> runOnTestData
    Assert.AreEqual(2, results.Count)
    Assert.AreEqual(h1, results.[0].Hash)
    Assert.AreEqual(h2, results.[1].Hash)

type FindFileHashByParameter = SQL<"""
insert into HashedBlobs(Hash) values(@seed1);
insert into HashedBlobs(Hash) values(@seed2);
select Hash from HashedBlobs where Hash = @needle;
""">

[<Test>]
let ``select with FileHash parameter equality returns the matching row only`` () =
    let target = FileHash [| 0xCAuy; 0xFEuy; 0xBAuy; 0xBEuy |]
    let other = FileHash [| 0xDEuy; 0xADuy; 0xBEuy; 0xEFuy |]
    let results = FindFileHashByParameter.Command(target, other, target) |> runOnTestData
    Assert.AreEqual(1, results.Count)
    Assert.AreEqual(target, results.[0].Hash)

type FindFileHashByOptionalParameter = SQL<"""
insert into HashedBlobs(Hash) values(@seed1);
insert into HashedBlobs(Hash) values(@seed2);
select Hash from HashedBlobs where Hash = @needle or @needle is null;
""">

[<Test>]
let ``select with optional FileHash parameter filters when Some and returns all when None`` () =
    let target = FileHash [| 0x12uy; 0x34uy; 0x56uy; 0x78uy |]
    let other = FileHash [| 0x9Auy; 0xBCuy; 0xDEuy; 0xF0uy |]
    // Rezoom orders Command args alphabetically by name: needle, seed1, seed2.
    let withSome =
        FindFileHashByOptionalParameter.Command(Some target, target, other)
        |> runOnTestData
    Assert.AreEqual(1, withSome.Count)
    Assert.AreEqual(target, withSome.[0].Hash)
    let withNone =
        FindFileHashByOptionalParameter.Command(None, target, other)
        |> runOnTestData
    Assert.AreEqual(2, withNone.Count)
