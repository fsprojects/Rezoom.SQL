module Rezoom.SQL.Test.TestTypeInference
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Compiler

let zeroModel =
    {   Schemas =
            [   Schema.Empty(Name("main"))
                Schema.Empty(Name("temp"))
            ] |> List.map (fun s -> s.SchemaName, s) |> Map.ofList
        DefaultSchema = Name("main")
        TemporarySchema = Name("temp")
        Builtin = { Functions = Map.empty }
        BackendCharacteristics =
            {   CanDropColumnWithDefaultValue = true
            }
    }

[<Test>]
let ``simple select`` () =
    let cmd = CommandEffect.OfSQL(zeroModel, "anonymous", @"
        create table Users(id int null primary key, name string(128) null, email string(128) null);
        select * from Users
    ")
    Assert.AreEqual(0, cmd.Parameters.Count)
    let results = cmd.ResultSets() |> toReadOnlyList
    Assert.AreEqual(1, results.Count)
    let cs = results.[0].Columns
    Assert.IsTrue(cs.[1].Expr.Info.PrimaryKey)
    Assert.AreEqual(Name("id"), cs.[1].ColumnName)
    Assert.AreEqual({ Nullable = true; Type = IntegerType Integer32 }, cs.[1].Expr.Info.Type)
    Assert.IsFalse(cs.[2].Expr.Info.PrimaryKey)
    Assert.AreEqual(Name("name"), cs.[2].ColumnName)
    Assert.AreEqual({ Nullable = true; Type = StringType }, cs.[2].Expr.Info.Type)
    Assert.IsFalse(cs.[0].Expr.Info.PrimaryKey)
    Assert.AreEqual(Name("email"), cs.[0].ColumnName)
    Assert.AreEqual({ Nullable = true; Type = StringType }, cs.[0].Expr.Info.Type)

[<Test>]
let ``simple select with parameter`` () =
    let cmd = CommandEffect.OfSQL(zeroModel, "anonymous", @"
        create table Users(id int null primary key, name string(128) null, email string(128) null);
        select * from Users u
        where u.id = @id
    ")
    Assert.AreEqual(1, cmd.Parameters.Count)
    Assert.AreEqual
        ( (NamedParameter (Name("id")), { Nullable = false; Type = IntegerType Integer32 })
        , cmd.Parameters.[0])
    let results = cmd.ResultSets() |> toReadOnlyList
    Assert.AreEqual(1, results.Count)
    let cs = results.[0].Columns
    Assert.IsTrue(cs.[1].Expr.Info.PrimaryKey)
    Assert.AreEqual(Name("id"), cs.[1].ColumnName)
    Assert.AreEqual({ Nullable = true; Type = IntegerType Integer32 }, cs.[1].Expr.Info.Type)
    Assert.IsFalse(cs.[2].Expr.Info.PrimaryKey)
    Assert.AreEqual(Name("name"), cs.[2].ColumnName)
    Assert.AreEqual({ Nullable = true; Type = StringType }, cs.[2].Expr.Info.Type)
    Assert.IsFalse(cs.[0].Expr.Info.PrimaryKey)
    Assert.AreEqual(Name("email"), cs.[0].ColumnName)
    Assert.AreEqual({ Nullable = true; Type = StringType }, cs.[0].Expr.Info.Type)

[<Test>]
let ``simple select with parameter nullable id`` () =
    let cmd = CommandEffect.OfSQL(zeroModel, "anonymous", @"
        create table Users(id int null primary key, name string(128) null, email string(128) null);
        select * from Users u
        where u.id is @id
    ")
    Assert.AreEqual(1, cmd.Parameters.Count)
    Assert.AreEqual
        ( (NamedParameter (Name("id")), { Nullable = true; Type = IntegerType Integer32 })
        , cmd.Parameters.[0])
    let results = cmd.ResultSets() |> toReadOnlyList
    Assert.AreEqual(1, results.Count)
    let cs = results.[0].Columns
    Assert.IsTrue(cs.[1].Expr.Info.PrimaryKey)
    Assert.AreEqual(Name("id"), cs.[1].ColumnName)
    Assert.AreEqual({ Nullable = true; Type = IntegerType Integer32 }, cs.[1].Expr.Info.Type)
    Assert.IsFalse(cs.[2].Expr.Info.PrimaryKey)
    Assert.AreEqual(Name("name"), cs.[2].ColumnName)
    Assert.AreEqual({ Nullable = true; Type = StringType }, cs.[2].Expr.Info.Type)
    Assert.IsFalse(cs.[0].Expr.Info.PrimaryKey)
    Assert.AreEqual(Name("email"), cs.[0].ColumnName)
    Assert.AreEqual({ Nullable = true; Type = StringType }, cs.[0].Expr.Info.Type)

[<Test>]
let ``simple select with parameter not null`` () =
    let cmd = 
        CommandEffect.OfSQL(zeroModel, "anonymous", @"
            create table Users(id int primary key, name string(128) null, email string(128) null);
            select * from Users u
            where u.id = @id
        ")
    Assert.AreEqual(1, cmd.Parameters.Count)
    Assert.AreEqual
        ( (NamedParameter (Name("id")), { Nullable = false; Type = IntegerType Integer32 })
        , cmd.Parameters.[0])
    let results = cmd.ResultSets() |> toReadOnlyList
    Assert.AreEqual(1, results.Count)
    let cs = results.[0].Columns
    Assert.IsTrue(cs.[1].Expr.Info.PrimaryKey)
    Assert.AreEqual(Name("id"), cs.[1].ColumnName)
    Assert.AreEqual({ Nullable = false; Type = IntegerType Integer32 }, cs.[1].Expr.Info.Type)
    Assert.IsFalse(cs.[2].Expr.Info.PrimaryKey)
    Assert.AreEqual(Name("name"), cs.[2].ColumnName)
    Assert.AreEqual({ Nullable = true; Type = StringType }, cs.[2].Expr.Info.Type)
    Assert.IsFalse(cs.[0].Expr.Info.PrimaryKey)
    Assert.AreEqual(Name("email"), cs.[0].ColumnName)
    Assert.AreEqual({ Nullable = true; Type = StringType }, cs.[0].Expr.Info.Type)

[<Test>]
let ``select where id in param`` () =
    let cmd = 
        CommandEffect.OfSQL(zeroModel, "anonymous", @"
            create table Users(id int primary key, name string(128), email string(128));
            select * from Users u
            where u.id in @id
        ")
    Assert.AreEqual(1, cmd.Parameters.Count)

[<Test>]
let ``coalesce not null`` () =
    let model = userModel1()
    let cmd = 
        CommandEffect.OfSQL(model.Model, "anonymous", @"
            select coalesce(u.Name, u.Email, @default) as c
            from Users u
            where u.id in @id
        ")
    printfn "%A" cmd.Parameters
    Assert.AreEqual(2, cmd.Parameters.Count)
    Assert.IsFalse((snd cmd.Parameters.[0]).Nullable)
    Assert.IsFalse((snd cmd.Parameters.[1]).Nullable)

[<Test>]
let ``coalesce null`` () =
    let model = userModel1()
    let cmd = 
        CommandEffect.OfSQL(model.Model, "anonymous", @"
            select coalesce(u.Name, @default, u.Email) as c
            from Users u
            where u.id in @id
        ")
    printfn "%A" cmd.Parameters
    Assert.AreEqual(2, cmd.Parameters.Count)
    Assert.IsTrue((snd cmd.Parameters.[0]).Nullable)
    Assert.IsFalse((snd cmd.Parameters.[1]).Nullable)

[<Test>]
let ``union null from bottom`` () =
    let model = userModel1()
    let cmd = 
        CommandEffect.OfSQL(model.Model, "anonymous", @"
            select 1 as x
            union all
            select null
        ")
    printfn "%A" cmd.Parameters
    Assert.AreEqual(0, cmd.Parameters.Count)
    let resultSets = cmd.ResultSets() |> Seq.toArray
    Assert.AreEqual(1, resultSets.Length)
    Assert.IsTrue(resultSets.[0].Columns.[0].Expr.Info.Type.Nullable)

[<Test>]
let ``union null from top`` () =
    let model = userModel1()
    let cmd = 
        CommandEffect.OfSQL(model.Model, "anonymous", @"
            select null as x
            union all
            select 1
        ")
    printfn "%A" cmd.Parameters
    Assert.AreEqual(0, cmd.Parameters.Count)
    let resultSets = cmd.ResultSets() |> Seq.toArray
    Assert.AreEqual(1, resultSets.Length)
    Assert.IsTrue(resultSets.[0].Columns.[0].Expr.Info.Type.Nullable)

[<Test>]
let ``union null in values clause`` () =
    let model = userModel1()
    let cmd = 
        CommandEffect.OfSQL(model.Model, "anonymous", @"
            select 1 as x
            union all
            values (null)
        ")
    printfn "%A" cmd.Parameters
    Assert.AreEqual(0, cmd.Parameters.Count)
    let resultSets = cmd.ResultSets() |> Seq.toArray
    Assert.AreEqual(1, resultSets.Length)
    Assert.IsTrue(resultSets.[0].Columns.[0].Expr.Info.Type.Nullable)

[<Test>]
let ``select max`` () =
    let model = userModel1()
    let cmd = 
        CommandEffect.OfSQL(model.Model, "anonymous", @"
            select max(Name) as MaxName from Users
        ")
    printfn "%A" cmd.Parameters
    Assert.AreEqual(0, cmd.Parameters.Count)
    let resultSets = cmd.ResultSets() |> Seq.toArray
    Assert.AreEqual(1, resultSets.Length)
    Assert.IsTrue(resultSets.[0].Columns.[0].Expr.Info.Type.Type = StringType)

[<Test>]
let ``correlated subquery`` () =
    let model = userModel1()
    let cmd = 
        CommandEffect.OfSQL(model.Model, "anonymous", @"
            select * from Users lu
            where exists(select null as x from Users ru where ru.Name = lu.Name || ' stuff')
        ")
    printfn "%A" cmd.Parameters
    Assert.AreEqual(0, cmd.Parameters.Count)
    let resultSets = cmd.ResultSets() |> Seq.toArray
    Assert.AreEqual(1, resultSets.Length)
    Assert.AreEqual(5, resultSets.[0].Columns.Count)
    Assert.IsTrue(resultSets.[0].Columns.[0].Expr.Info.Type.Type = StringType)

[<Test>]
let ``between expr`` () =
    { tsqlTest with
        Migration = ""
        Command = 
            """select 1 as it where
                sysutcdatetime() between sysutcdatetime() and sysutcdatetime()
            and 1 between 0 and 2
            and 'b' between 'a' and 'c';"""
        Expect =
            {   expect with
                    Idempotent = Some false
                    ResultSets = Some [ [ "it", { Type = NumericTypeClass; Nullable = false } ] ];
            } |> Good
    } |> assertSimple