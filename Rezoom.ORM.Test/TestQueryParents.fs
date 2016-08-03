namespace Rezoom.ORM.Test.QueryParents
open Rezoom.ORM
open Microsoft.VisualStudio.TestTools.UnitTesting

type RecordFolder =
    {
        Id : int
        Name : string
        Children : RecordFolder array
        QueryParent : RecordFolder
    }

[<AllowNullLiteral>]
type ClassFolder() =
    member val Id = 0 with get, set
    member val Name = "" with get, set
    member val Children = null : ClassFolder array with get, set
    member val QueryParent = null : ClassFolder with get, set

[<TestClass>]
type TestQueryParents() =
    [<TestMethod>]
    member __.TestRecordFolder() =
        let colMap =
            [|
                "Id", ColumnType.Int32
                "Name", ColumnType.String
                "Children$Id", ColumnType.Int32
                "Name", ColumnType.String
            |] |> ColumnMap.Parse
        let reader = ReaderTemplate<RecordFolder array>.Template().CreateReader()
        reader.ProcessColumns(colMap)
        for objectRow in
            [|
                ObjectRow(1, "A", 2, "A.1")
                ObjectRow(1, "A", 3, "A.2")
                ObjectRow(4, "B", 5, "B.1")
                ObjectRow(4, "B", 6, "B.2")
            |] do reader.Read(objectRow)
        let folders = reader.ToEntity()
        Assert.IsNotNull(folders)
        Assert.AreEqual(2, folders.Length)
        Assert.IsNotNull(folders.[0])
        Assert.IsNull(folders.[0].QueryParent)
        Assert.AreEqual(1, folders.[0].Id)
        Assert.AreEqual("A", folders.[0].Name)
        Assert.IsNotNull(folders.[0].Children)
        Assert.AreEqual(2, folders.[0].Children.Length)
        Assert.IsNotNull(folders.[0].Children.[0])
        Assert.AreEqual(2, folders.[0].Children.[0].Id)
        Assert.AreEqual("A.1", folders.[0].Children.[0].Name)
        Assert.IsNull(folders.[0].Children.[0].Children)
        Assert.IsNotNull(folders.[0].Children.[0].QueryParent)
        Assert.IsTrue(obj.ReferenceEquals(folders.[0], folders.[0].Children.[0].QueryParent))

    [<TestMethod>]
    member __.TestClassFolder() =
        let colMap =
            [|
                "Id", ColumnType.Int32
                "Name", ColumnType.String
                "Children$Id", ColumnType.Int32
                "Name", ColumnType.String
            |] |> ColumnMap.Parse
        let reader = ReaderTemplate<ClassFolder array>.Template().CreateReader()
        reader.ProcessColumns(colMap)
        let next = ReaderTemplate<ClassFolder array>.Template().CreateReader() 
        reader.ImpartKnowledgeToNext(next)
        for objectRow in
            [|
                ObjectRow(1, "A", 2, "A.1")
                ObjectRow(1, "A", 3, "A.2")
                ObjectRow(4, "B", 5, "B.1")
                ObjectRow(4, "B", 6, "B.2")
            |] do reader.Read(objectRow)
        let folders = reader.ToEntity()
        Assert.IsNotNull(folders)
        Assert.AreEqual(2, folders.Length)
        Assert.IsNotNull(folders.[0])
        Assert.IsNull(folders.[0].QueryParent)
        Assert.AreEqual(1, folders.[0].Id)
        Assert.AreEqual("A", folders.[0].Name)
        Assert.IsNotNull(folders.[0].Children)
        Assert.AreEqual(2, folders.[0].Children.Length)
        Assert.IsNotNull(folders.[0].Children.[0])
        Assert.AreEqual(2, folders.[0].Children.[0].Id)
        Assert.AreEqual("A.1", folders.[0].Children.[0].Name)
        Assert.IsNull(folders.[0].Children.[0].Children)
        Assert.IsNotNull(folders.[0].Children.[0].QueryParent)
        Assert.IsTrue(obj.ReferenceEquals(folders.[0], folders.[0].Children.[0].QueryParent))