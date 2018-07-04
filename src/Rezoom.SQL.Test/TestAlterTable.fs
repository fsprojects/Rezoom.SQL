module Rezoom.SQL.Test.AlterTable
open Rezoom.SQL.Compiler
open NUnit.Framework

[<Test>]
let ``adding default permits insert without column value`` () =
    { defaultTest with
        Migration = """
            create table Foo(x int, y int);
            alter table Foo add default for x 1
        """
        Command = "insert into Foo row y = 2"
        Expect = Good expect
    } |> assertSimple

[<Test>]
let ``removing default denies insert without column value`` () =
    { defaultTest with
        Migration = """
            create table Foo(x int, y int);
            alter table Foo add default for x 1;
            alter table Foo drop default for x
        """
        Command = "insert into Foo row y = 2"
        Expect = BadCommand <| Error.insertMissingColumns ["x"]
    } |> assertSimple

[<Test>]
let ``can't add same constraint name to different tables`` () =
    { defaultTest with
        Migration = """
            create table Foo(x int constraint nm primary key, y int);
            create table Bar(z int constraint nm primary key);
        """
        Expect = BadMigration <| Error.objectAlreadyExists "main.nm"
    } |> assertSimple

[<Test>]
let ``can add same constraint name to different table after dropping`` () =
    { defaultTest with
        Migration = """
            create table Foo(x int constraint nm primary key, y int);
            alter table Foo drop constraint nm;
            create table Bar(z int constraint nm primary key);
        """
        Expect = expect |> Good
    } |> assertSimple

[<Test>]
let ``can't create same index name on two tables`` () =
    { defaultTest with
        Migration = """
            create table Foo(x int);
            create index IX_Example on Foo(x);
            create table Bar(y int);
            create index IX_Example on Bar(y);
        """
        Expect = BadMigration <| Error.objectAlreadyExists "main.IX_Example"
    } |> assertSimple

[<Test>]
let ``can create same index name after dropping first table`` () =
    { defaultTest with
        Migration = """
            create table Foo(x int);
            create index IX_Example on Foo(x);
            create table Bar(y int);
            drop table Foo;
            create index IX_Example on Bar(y);
        """
        Expect = expect |> Good
    } |> assertSimple

[<Test>]
let ``string can't be autoincrement`` () =
    { defaultTest with
        Migration = "create table Foo(x string(10) primary key autoincrement)"
        Expect = BadMigration <| Error.onlyIntPrimaryKeyAutoincrement
    } |> assertSimple

[<Test>]
let ``can't drop last column`` () =
    { defaultTest with
        Migration = "create table Foo(x int); alter table Foo drop column x"
        Expect = BadMigration <| Error.cannotDropLastColumn "main.Foo" "x"
    } |> assertSimple

[<Test>]
let ``can't alter pk column`` () =
    { defaultTest with
        Migration = "create table Foo(x int primary key); alter table Foo alter column x int64"
        Expect = BadMigration <| Error.cannotAlterPrimaryKeyColumn "x"
    } |> assertSimple

[<Test>]
let ``can't collate guid`` () =
    { defaultTest with
        Migration = "create table Foo(x guid collate foo);"
        Expect = BadMigration <| Error.cannotCollateType "GUID"
    } |> assertSimple