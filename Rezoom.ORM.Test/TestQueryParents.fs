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
    [<Ignore>] // Expected to always stack overflow and crash VS test engine. Fun to run only as a curiousity.
    member __.TestRecordEquality() =
        let colMap =
            [|
                "Id", ColumnType.Int32
                "Name", ColumnType.String
                "Children$Id", ColumnType.Int32
                "Name", ColumnType.String
                "Children$Children$Id", ColumnType.Int32
                "Name", ColumnType.String
            |] |> ColumnMap.Parse
        let reader = ReaderTemplate<RecordFolder list>.Template().CreateReader()
        reader.ProcessColumns(colMap)
        for objectRow in
            [|
                ObjectRow(1, "A", 2, "A.1", 7, "A.1.1")
                ObjectRow(1, "A", 3, "A.2", 8, "A.2.1")
                ObjectRow(4, "B", 5, "B.1", 9, "B.1.1")
                ObjectRow(4, "B", 6, "B.2", 10, "B.2.1")
            |] do reader.Read(objectRow)
        let folders1 = reader.ToEntity()

        let reader = ReaderTemplate<RecordFolder list>.Template().CreateReader()
        reader.ProcessColumns(colMap)
        for objectRow in
            [|
                ObjectRow(1, "A", 2, "A.1", 7, "A.1.1")
                ObjectRow(1, "A", 3, "A.2", 8, "A.2.1")
                ObjectRow(4, "B", 5, "B.1", 9, "B.1.1")
                ObjectRow(4, "B", 6, "B.2", 10, "B.2.1")
            |] do reader.Read(objectRow)
        let folders2 = reader.ToEntity()
        let bottom1 = folders1.Head.Children.[0].Children.[0]
        let bottom2 = folders2.Head.Children.[0].Children.[0]
        Assert.AreEqual(bottom1, bottom2)
        // This will stack overflow, because the equality comparison goes:
        // are we equal? -->
        // ^    are our Ids equal? yes.
        // |    are our Names equal? yes.
        // ^    are our Children equal? yes.
        // |    are our Parents equal? -->
        // ^        are their Ids equal? yes.
        // |        are their Names equal? yes.
        // ^        are their Children equal? -->+
        // |                                     |
        // +-<--<--<--<--<--<--<--<--<--<--<--<--+

        // There is nothing we can do about this other than advise against using records this way.
        // This test serves as a handy way of demonstrating the problem.

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

        Assert.IsNotNull(folders.[0].Children.[1])
        Assert.AreEqual(3, folders.[0].Children.[1].Id)
        Assert.AreEqual("A.2", folders.[0].Children.[1].Name)
        Assert.IsNull(folders.[0].Children.[1].Children)
        Assert.IsNotNull(folders.[0].Children.[1].QueryParent)
        Assert.IsTrue(obj.ReferenceEquals(folders.[0], folders.[0].Children.[1].QueryParent))

        Assert.IsNotNull(folders.[1])
        Assert.IsNull(folders.[1].QueryParent)
        Assert.AreEqual(4, folders.[1].Id)
        Assert.AreEqual("B", folders.[1].Name)
        Assert.IsNotNull(folders.[1].Children)
        Assert.AreEqual(2, folders.[1].Children.Length)

        Assert.IsNotNull(folders.[1].Children.[0])
        Assert.AreEqual(5, folders.[1].Children.[0].Id)
        Assert.AreEqual("B.1", folders.[1].Children.[0].Name)
        Assert.IsNull(folders.[1].Children.[0].Children)
        Assert.IsNotNull(folders.[1].Children.[0].QueryParent)
        Assert.IsTrue(obj.ReferenceEquals(folders.[1], folders.[1].Children.[0].QueryParent))

        Assert.IsNotNull(folders.[1].Children.[1])
        Assert.AreEqual(6, folders.[1].Children.[1].Id)
        Assert.AreEqual("B.2", folders.[1].Children.[1].Name)
        Assert.IsNull(folders.[1].Children.[1].Children)
        Assert.IsNotNull(folders.[1].Children.[1].QueryParent)
        Assert.IsTrue(obj.ReferenceEquals(folders.[1], folders.[1].Children.[1].QueryParent))

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

        Assert.IsNotNull(folders.[0].Children.[1])
        Assert.AreEqual(3, folders.[0].Children.[1].Id)
        Assert.AreEqual("A.2", folders.[0].Children.[1].Name)
        Assert.IsNull(folders.[0].Children.[1].Children)
        Assert.IsNotNull(folders.[0].Children.[1].QueryParent)
        Assert.IsTrue(obj.ReferenceEquals(folders.[0], folders.[0].Children.[1].QueryParent))

        Assert.IsNotNull(folders.[1])
        Assert.IsNull(folders.[1].QueryParent)
        Assert.AreEqual(4, folders.[1].Id)
        Assert.AreEqual("B", folders.[1].Name)
        Assert.IsNotNull(folders.[1].Children)
        Assert.AreEqual(2, folders.[1].Children.Length)

        Assert.IsNotNull(folders.[1].Children.[0])
        Assert.AreEqual(5, folders.[1].Children.[0].Id)
        Assert.AreEqual("B.1", folders.[1].Children.[0].Name)
        Assert.IsNull(folders.[1].Children.[0].Children)
        Assert.IsNotNull(folders.[1].Children.[0].QueryParent)
        Assert.IsTrue(obj.ReferenceEquals(folders.[1], folders.[1].Children.[0].QueryParent))

        Assert.IsNotNull(folders.[1].Children.[1])
        Assert.AreEqual(6, folders.[1].Children.[1].Id)
        Assert.AreEqual("B.2", folders.[1].Children.[1].Name)
        Assert.IsNull(folders.[1].Children.[1].Children)
        Assert.IsNotNull(folders.[1].Children.[1].QueryParent)
        Assert.IsTrue(obj.ReferenceEquals(folders.[1], folders.[1].Children.[1].QueryParent))