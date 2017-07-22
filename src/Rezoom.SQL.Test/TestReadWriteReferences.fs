module Rezoom.SQL.Test.ReadWriteReferences
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Mapping
open Rezoom.SQL.Mapping.CodeGeneration

[<Test>]
let ``deletes cascade`` () =
    { sqliteTest with
        Migration = """
            create table Parent(Id int primary key);
            create table Child(Id int primary key, ParentId int references Parent(Id) on delete cascade);
            create table GrandChild(Id int primary key, ParentId int references Child(Id) on delete cascade);
        """
        Command = "delete from Parent"
        Expect =
            { expect with
                Idempotent = Some false
                WriteTables = Some [ "main.Parent"; "main.Child"; "main.GrandChild" ]
            } |> Good
    } |> assertSimple

[<Test>]
let ``deletes stop cascade at set null`` () =
    { sqliteTest with
        Migration = """
            create table Parent(Id int primary key);
            create table Child(Id int primary key, ParentId int references Parent(Id) on delete set null);
            create table GrandChild(Id int primary key, ParentId int references Child(Id) on delete cascade);
        """
        Command = "delete from Parent"
        Expect =
            { expect with
                Idempotent = Some false
                WriteTables = Some [ "main.Parent"; "main.Child" ]
            } |> Good
    } |> assertSimple