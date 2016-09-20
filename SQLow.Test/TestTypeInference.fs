namespace SQLow.Test
open Microsoft.VisualStudio.TestTools.UnitTesting
open SQLow

[<TestClass>]
type TestTypeInference() =
    static let zeroSchema name =
        {   SchemaName = name
            Tables = Map.empty
            Views = Map.empty
        }
    static let zeroModel =
        {   Schemas =
                [   zeroSchema (Name("main"))
                    zeroSchema (Name("temp"))
                ] |> List.map (fun s -> s.SchemaName, s) |> Map.ofList
            DefaultSchema = Name("main")
            TemporarySchema = Name("temp")
            Builtin = { Functions = Map.empty }
        }
    [<TestMethod>]
    member __.TestSimpleSelect() =
        let cmd = zeroModel |> CommandEffect.ofSql @"
            create table Users(id int primary key, name nvarchar(128), email nvarchar(128));
            select * from Users
        "
        printfn "%A" cmd
        Assert.AreEqual(0, cmd.Parameters.Count)
        Assert.AreEqual(1, cmd.ResultSets.Count)
        let cs = cmd.ResultSets.[0].Columns
        Assert.IsTrue(cs.[0].PrimaryKey)
        Assert.AreEqual(Name("id"), cs.[0].ColumnName)
        Assert.AreEqual({ Nullable = true; Type = IntegerType }, cs.[0].ColumnType)
        Assert.IsFalse(cs.[1].PrimaryKey)
        Assert.AreEqual(Name("name"), cs.[1].ColumnName)
        Assert.AreEqual({ Nullable = true; Type = StringType }, cs.[1].ColumnType)
        Assert.IsFalse(cs.[2].PrimaryKey)
        Assert.AreEqual(Name("email"), cs.[2].ColumnName)
        Assert.AreEqual({ Nullable = true; Type = StringType }, cs.[2].ColumnType)

    [<TestMethod>]
    member __.TestSimpleSelectWithParameter() =
        let cmd = zeroModel |> CommandEffect.ofSql @"
            create table Users(id int primary key, name nvarchar(128), email nvarchar(128));
            select * from Users u
            where u.id = @id
        "
        printfn "%A" cmd
        Assert.AreEqual(1, cmd.Parameters.Count)
        Assert.AreEqual
            ( (NamedParameter (Name("id")), { Nullable = true; Type = IntegerType })
            , cmd.Parameters.[0])
        Assert.AreEqual(1, cmd.ResultSets.Count)
        let cs = cmd.ResultSets.[0].Columns
        Assert.IsTrue(cs.[0].PrimaryKey)
        Assert.AreEqual(Name("id"), cs.[0].ColumnName)
        Assert.AreEqual({ Nullable = true; Type = IntegerType }, cs.[0].ColumnType)
        Assert.IsFalse(cs.[1].PrimaryKey)
        Assert.AreEqual(Name("name"), cs.[1].ColumnName)
        Assert.AreEqual({ Nullable = true; Type = StringType }, cs.[1].ColumnType)
        Assert.IsFalse(cs.[2].PrimaryKey)
        Assert.AreEqual(Name("email"), cs.[2].ColumnName)
        Assert.AreEqual({ Nullable = true; Type = StringType }, cs.[2].ColumnType)

    [<TestMethod>]
    member __.TestSimpleSelectWithParameterNotNull() =
        let cmd = zeroModel |> CommandEffect.ofSql @"
            create table Users(id int primary key not null, name nvarchar(128), email nvarchar(128));
            select * from Users u
            where u.id = @id
        "
        printfn "%A" cmd
        Assert.AreEqual(1, cmd.Parameters.Count)
        Assert.AreEqual
            ( (NamedParameter (Name("id")), { Nullable = false; Type = IntegerType })
            , cmd.Parameters.[0])
        Assert.AreEqual(1, cmd.ResultSets.Count)
        let cs = cmd.ResultSets.[0].Columns
        Assert.IsTrue(cs.[0].PrimaryKey)
        Assert.AreEqual(Name("id"), cs.[0].ColumnName)
        Assert.AreEqual({ Nullable = false; Type = IntegerType }, cs.[0].ColumnType)
        Assert.IsFalse(cs.[1].PrimaryKey)
        Assert.AreEqual(Name("name"), cs.[1].ColumnName)
        Assert.AreEqual({ Nullable = true; Type = StringType }, cs.[1].ColumnType)
        Assert.IsFalse(cs.[2].PrimaryKey)
        Assert.AreEqual(Name("email"), cs.[2].ColumnName)
        Assert.AreEqual({ Nullable = true; Type = StringType }, cs.[2].ColumnType)
