module Rezoom.SQL.Test.TestTSQL
open System
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Compiler
open Rezoom.SQL.Mapping

let translate (sql : string) (expectedTSQL : string) =
    let userModel =
        let userModel = userModel1()
        let backend = TSQL.TSQLBackend() :> IBackend
        { userModel with
            Backend = backend
            Model = { userModel.Model with Builtin = backend.InitialModel.Builtin }
        }
    let parsed = CommandEffect.OfSQL(userModel.Model, "anonymous", sql)
    let indexer = { new IParameterIndexer with member __.ParameterIndex(_) = 0 }
    let fragments = userModel.Backend.ToCommandFragments(indexer, parsed.Statements)
    let str = CommandFragment.Stringize(fragments)
    Console.WriteLine(str)
    Assert.AreEqual(expectedTSQL.SmushWhitespace(), str.SmushWhitespace())

[<Test>]
let ``at at proc translation`` () =
    translate
        """select datefirst() as d"""
        """SELECT @@DATEFIRST AS [d];"""

[<Test>]
let ``datepart translation`` () =
    translate
        """select dateadd('day', 1, sysutcdatetime()) d"""
        """SELECT dateadd(day,1,sysutcdatetime()) AS [d];"""

[<Test>]
let ``bool to first class`` ()=
    translate
        """select 1 < 0 as b"""
        """SELECT CAST((CASE WHEN (1 < 0) THEN 1 ELSE 0 END) AS BIT) AS [b];"""

[<Test>]
let ``first class to bool`` ()=
    translate
        """select 1 as col from Users where true"""
        """SELECT 1 AS [col] FROM [Users] WHERE ((1)<>0);"""

[<Test>]
let ``iif with predicate`` ()=
    translate
        """select IIF(1 > 0, 'a', 'b') as choice"""
        """SELECT IIF((1 > 0),N'a',N'b') AS [choice];"""

[<Test>]
let ``iif with first class value`` () =
    translate
        """select IIF(false, 'a', 'b') as choice"""
        """SELECT IIF(((0)<>0),N'a',N'b') AS [choice];"""

[<Test>]
let ``temp table`` () =
    translate
        """create temp table x(a int);"""
        """CREATE TABLE [#x] ( [a] INT NOT NULL );"""

[<Test>]
let ``temp table from select`` () =
    translate
        """create temp table x as select Id, Email from Users where Id > 0"""
        ( """SELECT * INTO [#x] FROM (SELECT [Users].[Id] , [Users].[Email]"""
        + """ FROM [Users] WHERE ([Users].[Id] > 0)) __rzsubquery;""")

[<Test>]
let ``create xusers`` () =
    translate
        """
create table XUsers
    ( Id int primary key autoincrement
    , Email string(254) unique
    , Name string(64) null
    );
        """
        ( "CREATE TABLE [XUsers] "
        + "( [Id] INT NOT NULL CONSTRAINT [XUsers_Id_PK] PRIMARY KEY IDENTITY(1,1)"
        + " , [Email] NVARCHAR(254) NOT NULL CONSTRAINT [XUsers_Email_UNIQUE] UNIQUE"
        + " , [Name] NVARCHAR(64) );")

[<Test>]
let ``create xusers table constraints`` () =
    translate
        """
create table XUsers
    ( Id int
    , Email string(254)
    , Name string(64) null
    , unique(Email, Name)
    , primary key(Id)
    , check(Id > 0)
    );
        """
        ( "CREATE TABLE [XUsers] "
        + "( [Id] INT NOT NULL"
        + " , [Email] NVARCHAR(254) NOT NULL"
        + " , [Name] NVARCHAR(64)"
        + " , CONSTRAINT [XUsers_Email_Name_UNIQUE] UNIQUE([Email] ASC,[Name] ASC)"
        + " , CONSTRAINT [XUsers_Id_PK] PRIMARY KEY([Id] ASC)"
        + " , CONSTRAINT [XUsers_CHECK] CHECK(([Id] > 0)) );")

[<Test>]
let ``select top`` () =
    translate
        "select 1 as x from Users limit 5"
        "SELECT TOP (5) 1 AS [x] FROM [Users];"

[<Test>]
let ``select offset`` () =
    translate
        "select 1 as x from Users limit 5 offset 10"
        "SELECT 1 AS [x] FROM [Users] OFFSET 10 ROWS FETCH NEXT 5 ROWS ONLY;"


[<Test>]
let ``insert row`` () =
    translate
        "insert into Users row Email = 'email@example.com', Name = 'name'"
        "INSERT INTO [Users] ([Email],[Name]) VALUES (N'email@example.com',N'name');"

[<Test>]
let ``alter table scenarios`` () =
    translate
        """
create table Foo(x int primary key, y int);
alter table Foo add default for y 1;
alter table Foo drop constraint Foo_x_PK;
alter table Foo add constraint namedpk primary key(x, y);
alter table Foo drop constraint namedpk;
alter table Foo drop default for y;
alter table Foo alter column y string(12);
alter table Foo alter column y null;
alter table Foo add column z string(80) null collate SQL_Latin1_General_CP1_CI_AS default('zzz');
        """
        // below confirmed to be valid on SQL server 2014
        ("""
CREATE TABLE [Foo] ( [x] INT NOT NULL CONSTRAINT [Foo_x_PK] PRIMARY KEY , [y] INT NOT NULL );
ALTER TABLE [Foo] ADD CONSTRAINT [Foo_y_DEFAULT_CONSTRAINT] DEFAULT 1 FOR [y];
ALTER TABLE [Foo] DROP CONSTRAINT [Foo_x_PK];
ALTER TABLE [Foo] ADD CONSTRAINT [namedpk] PRIMARY KEY([x] ASC,[y] ASC);
ALTER TABLE [Foo] DROP CONSTRAINT [namedpk];
ALTER TABLE [Foo] DROP CONSTRAINT [Foo_y_DEFAULT_CONSTRAINT];
ALTER TABLE [Foo] ALTER COLUMN [y] NVARCHAR(12) NOT NULL;
ALTER TABLE [Foo] ALTER COLUMN [y] NVARCHAR(12) NULL;
ALTER TABLE [Foo] ADD [z] NVARCHAR(80) COLLATE SQL_Latin1_General_CP1_CI_AS CONSTRAINT [Foo_z_DEFAULT_CONSTRAINT] DEFAULT N'zzz';
        """.SmushWhitespace())

[<Test>]
let ``tsql dropping column with default yells`` () =
    { tsqlTest with
        Migration =
            """
create table foo(x int default 0, y int);
alter table foo drop column x;
            """
        Expect =
            BadMigration <| Error.cannotDropColumnWithDefault "x"
    } |> assertSimple

[<Test>]
let ``tsql dropping column after dropping default is ok`` () =
    { tsqlTest with
        Migration =
            """
create table foo(x int default 0, y int);
alter table foo drop default for x;
alter table foo drop column x;
            """
        Expect = expect |> Good
    } |> assertSimple

[<Test>]
let ``tsql dump function signatures`` () =
    for KeyValue(_, func) in tsqlTest.TestBackend.InitialModel.Builtin.Functions do
        printfn "%s" (dumpSignature func)
        printfn ""