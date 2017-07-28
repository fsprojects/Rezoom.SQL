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
        Command = ""
        Expect = BadCommand <| Error.objectAlreadyExists "main.nm"
    } |> assertSimple

[<Test>]
let ``can add same constraint name to different table after dropping`` () =
    { defaultTest with
        Migration = """
            create table Foo(x int constraint nm primary key, y int);
            alter table Foo drop constraint nm;
            create table Bar(z int constraint nm primary key);
        """
        Command = ""
        Expect = expect |> Good
    } |> assertSimple