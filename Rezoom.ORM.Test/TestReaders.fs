namespace Rezoom.ORM.Test
open Rezoom.ORM
open Microsoft.VisualStudio.TestTools.UnitTesting

type User = 
    {
        Id : int
        Name : string
    }

[<TestClass>]
type TestReaders() =
    [<TestMethod>]
    member __.TestReadUser() =
        let colMap =
            [|
                "Id", ColumnType.Int32
                "Name", ColumnType.String
            |] |> ColumnMap.Parse
        let row = ObjectRow(1, "jim")
        let reader = ReaderTemplate<User>.Template().CreateReader()
        reader.ProcessColumns(colMap)
        reader.Read(row)
        let user = reader.ToEntity()
        Assert.IsNotNull(user)
        Assert.AreEqual(1, user.Id)
        Assert.AreEqual("jim", user.Name)

            