module Rezoom.SQL.Test.TestLevenshtein
open Rezoom.SQL.Mapping.Levenshtein
open System
open NUnit.Framework
open FsUnit

[<Test>]
let ``test algo equivalence`` () =
    let sampleString =
        [|  "foo"
            "food"
            "bar"
            "barf"
            "crOwbar"
            "barfoOd"
            "barfood"
            "crowfoot"
            ""
            "Food"
            "Cars"
            "cRoWsNesT"
            "cram"
            "cost"
        |]
    for s1 in sampleString do
        for s2 in sampleString do
            let dist = Slow.distance s1 s2
            let fastDist = distance s1 s2
            Assert.AreEqual(dist, fastDist)
            let ciDist = Slow.distanceCI s1 s2
            let ciFastDist = distanceCI s1 s2
            Assert.AreEqual(ciDist, ciFastDist)

[<Test>]
let ``empty string distance`` () =
    Assert.AreEqual(0, distance "" "")
    Assert.AreEqual(1, distance "x" "")
    Assert.AreEqual(1, distance "" "x")
    Assert.AreEqual(5, distance "abcde" "")
    Assert.AreEqual(5, distance "" "abcde")


[<Test>]
let ``case based string distance`` () =
    Assert.AreEqual(1, distance "abcde" "abCde")
    Assert.AreEqual(5, distance "ABCDE" "abcde")
    Assert.AreEqual(0, distanceCI "abcde" "abCde")
    Assert.AreEqual(0, distanceCI "ABCDE" "abcde")
    Assert.AreEqual(1, distanceCI "ABCDE" "abce")
    Assert.AreEqual(1, distanceCI "BCDE" "abcde")