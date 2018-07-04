module Rezoom.SQL.Test.ManyPrimitives
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Mapping
open Rezoom.SQL.Mapping.CodeGeneration
open System
open System.Collections.Generic

type Friend = 
    {   Id : int
        Name : string
        Aliases : string array
    }

[<Test>]
let ``read friend`` () =
    let colMap =
        [|  "Id", ColumnType.Int32
            "Name", ColumnType.String
            "Aliases", ColumnType.String
        |] |> ColumnMap.Parse
    let rows =
        [   ObjectRow(3, "Robert", "Bob")
            ObjectRow(3, "Robert", "Bobby")
            ObjectRow(3, "Robert", "Rob")
            ObjectRow(3, "Robert", "Robby")
        ]
    let reader = ReaderTemplate<Friend>.Template().CreateReader()
    reader.ProcessColumns(colMap)
    for row in rows do
        reader.Read(row)
    let friend = reader.ToEntity()
    Assert.IsNotNull(friend)
    Assert.AreEqual(3, friend.Id)
    Assert.AreEqual("Robert", friend.Name)
    Assert.AreEqual(4, friend.Aliases.Length)
    Assert.IsTrue([| "Bob"; "Bobby"; "Rob"; "Robby" |] = friend.Aliases)


type StringPair = // notice no key properties
    {   Left : string
        Right : string
    }

[<Test>]
let ``read string pairs`` () =
    let colMap =
        [|  "Left", ColumnType.String
            "Right", ColumnType.String
        |] |> ColumnMap.Parse
    let rows =
        [   ObjectRow("a", "1")
            ObjectRow("b", "2")
            ObjectRow("b", "2") // duplicate should appear in results
            ObjectRow("a", "1")
        ]
    let reader = ReaderTemplate<StringPair array>.Template().CreateReader()
    reader.ProcessColumns(colMap)
    for row in rows do
        reader.Read(row)
    let pairs = reader.ToEntity()
    Assert.AreEqual
        (   [|  { Left = "a"; Right = "1" }
                { Left = "b"; Right = "2" }
                { Left = "b"; Right = "2" }
                { Left = "a"; Right = "1" }
            |]
        , pairs
        )

[<BlueprintNoKey>]
type IgnoredIds =
    {   [<BlueprintKey>]
        Le : string
        Ri : string
    }

[<Test>]
let ``ignored ids`` () =
    let colMap =
        [|  "Le", ColumnType.String
            "Ri", ColumnType.String
        |] |> ColumnMap.Parse
    let rows =
        [   ObjectRow("a", "1")
            ObjectRow("b", "2")
            ObjectRow("b", "2") // duplicate should appear in results
            ObjectRow("a", "1")
        ]
    let reader = ReaderTemplate<IgnoredIds IReadOnlyList>.Template().CreateReader()
    reader.ProcessColumns(colMap)
    for row in rows do
        reader.Read(row)
    let pairs = reader.ToEntity()
    Assert.AreEqual
        (   [|  { Le = "a"; Ri = "1" }
                { Le = "b"; Ri = "2" }
                { Le = "b"; Ri = "2" }
                { Le = "a"; Ri = "1" }
            |]
        , pairs |> Array.ofSeq
        )