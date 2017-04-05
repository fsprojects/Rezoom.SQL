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
    Assert.AreEqual(expectedTSQL, str)

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
        """SELECT IIF((1 > 0),'a','b') AS [choice];"""

[<Test>]
let ``iif with first class value`` () =
    translate
        """select IIF(false, 'a', 'b') as choice"""
        """SELECT IIF(((0)<>0),'a','b') AS [choice];"""

[<Test>]
let ``temp table`` () =
    translate
        """create temp table x(a int);"""
        """CREATE TABLE [#x] ([a] INT CONSTRAINT [x_a_NOTNULL] NOT NULL);"""

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
        + "([Id] INT CONSTRAINT [XUsers_Id_NOTNULL] NOT NULL CONSTRAINT [XUsers_Id_PK] PRIMARY KEY IDENTITY(1,1)"
        + " , [Email] NVARCHAR(254) CONSTRAINT [XUsers_Email_NOTNULL] NOT NULL CONSTRAINT [XUsers_Email_UNIQUE] UNIQUE"
        + " , [Name] NVARCHAR(64) CONSTRAINT [XUsers_Name_NULL] NULL);")

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
        + "([Id] INT CONSTRAINT [XUsers_Id_NOTNULL] NOT NULL"
        + " , [Email] NVARCHAR(254) CONSTRAINT [XUsers_Email_NOTNULL] NOT NULL"
        + " , [Name] NVARCHAR(64) CONSTRAINT [XUsers_Name_NULL] NULL"
        + " , CONSTRAINT [XUsers_Email_Name_UNIQUE] UNIQUE([Email] ASC,[Name] ASC)"
        + " , CONSTRAINT [XUsers_Id_PK] PRIMARY KEY([Id] ASC)"
        + " , CONSTRAINT [XUsers_CHECK] CHECK(([Id] > 0)));")