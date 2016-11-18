module Rezoom.SQL.Test.CompositeReaders
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Mapping
open Rezoom.SQL.Mapping.CodeGeneration
open System
open System.ComponentModel.DataAnnotations

type User = 
    {
        UserId : int
        Name : string
    }

type Folder =
    {
        FolderId : int
        Children : Folder array
    }

type Person =
    {
        PersonId : int
        Name : string
        Parent : Person
    }

type CompositeKeyType =
    {
        [<BlueprintKey>]
        FooId : int
        [<BlueprintKey>]
        BarId : int
        MapName : string
    }

[<Test>]
let ``read user`` () =
    let colMap =
        [|
            "UserId", ColumnType.Int32
            "Name", ColumnType.String
        |] |> ColumnMap.Parse
    let row = ObjectRow(1, "jim")
    let reader = ReaderTemplate<User>.Template().CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(row)
    let user = reader.ToEntity()
    Assert.IsNotNull(user)
    Assert.AreEqual(1, user.UserId)
    Assert.AreEqual("jim", user.Name)

[<Test>]
let ``read many users`` () =
    let colMap =
        [|
            "UserId", ColumnType.Int32
            "Name", ColumnType.String
        |] |> ColumnMap.Parse
    let reader = ReaderTemplate<User list>.Template().CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(ObjectRow(1, "jim"))
    reader.Read(ObjectRow(1, "jim"))
    reader.Read(ObjectRow(2, "jerry"))
    let users = reader.ToEntity()
    Assert.AreEqual(
        [
            { UserId = 1; Name = "jim" }
            { UserId = 2; Name = "jerry" }
        ],
        users)

[<Test>]
let ``read folder 1 level deep`` () =
    let colMap =
        [|
            "FolderId", ColumnType.Int32
        |] |> ColumnMap.Parse
    let reader = ReaderTemplate<Folder>.Template().CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(ObjectRow(1))
    let folder = reader.ToEntity()
    Assert.IsNotNull(folder)
    Assert.AreEqual(1, folder.FolderId)
    Assert.AreEqual(0, folder.Children.Length)

[<Test>]
let ``read folder 2 levels deep`` () =
    let colMap =
        [|
            "FolderId", ColumnType.Int32
            "Children.FolderId", ColumnType.Int32
        |] |> ColumnMap.Parse
    let reader = ReaderTemplate<Folder>.Template().CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(ObjectRow(1, 2))
    reader.Read(ObjectRow(1, 3))
    let folder = reader.ToEntity()
    Assert.IsNotNull(folder)
    Assert.AreEqual(1, folder.FolderId)
    Assert.AreEqual(2, folder.Children.Length)
    Assert.AreEqual(2, folder.Children.[0].FolderId)
    Assert.AreEqual(3, folder.Children.[1].FolderId)
    Assert.AreEqual(0, folder.Children.[0].Children.Length)
    Assert.AreEqual(0, folder.Children.[1].Children.Length)

[<Test>]
let ``read person 1 level deep`` () =
    let colMap =
        [|
            "PersonId", ColumnType.Int32
            "Name", ColumnType.String
        |] |> ColumnMap.Parse
    let reader = ReaderTemplate<Person>.Template().CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(ObjectRow(1, "ben"))
    let person = reader.ToEntity()
    Assert.IsNotNull(person)
    Assert.AreEqual(1, person.PersonId)
    Assert.AreEqual("ben", person.Name)
    Assert.IsNull(person.Parent)

[<Test>]
let ``read person 2 levels deep`` () =
    let colMap =
        [|
            "PersonId", ColumnType.Int32
            "Name", ColumnType.String
            "Parent.PersonId", ColumnType.Int32
            "Name", ColumnType.String
        |] |> ColumnMap.Parse
    let reader = ReaderTemplate<Person>.Template().CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(ObjectRow(1, "ben", 2, "pat"))
    let person = reader.ToEntity()
    Assert.IsNotNull(person)
    Assert.AreEqual(1, person.PersonId)
    Assert.AreEqual("ben", person.Name)
    Assert.IsNotNull(person.Parent)
    Assert.AreEqual(2, person.Parent.PersonId)
    Assert.AreEqual("pat", person.Parent.Name)

[<Test>]
let ``read objects with composite keys`` () =
    let colMap =
        [|
            "FooId", ColumnType.Int32
            "BarId", ColumnType.Int32
            "MapName", ColumnType.String
        |] |> ColumnMap.Parse
    let reader = ReaderTemplate<CompositeKeyType list>.Template().CreateReader()
    reader.ProcessColumns(colMap)
    reader.Read(ObjectRow(1, 1, "a"))
    reader.Read(ObjectRow(1, 1, "b")) // should be ignored
    reader.Read(ObjectRow(1, 2, "c"))
    reader.Read(ObjectRow(2, 1, "d"))
    reader.Read(ObjectRow(2, 2, "e"))
    let composites = reader.ToEntity()
    Assert.AreEqual
        ([
            { FooId = 1; BarId = 1; MapName = "a" }
            { FooId = 1; BarId = 2; MapName = "c" }
            { FooId = 2; BarId = 1; MapName = "d" }
            { FooId = 2; BarId = 2; MapName = "e" }
        ], composites)
            