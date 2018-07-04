module Rezoom.SQL.Test.TestModelErrors
open System
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Compiler
open Rezoom.SQL.Mapping

[<Test>]
let ``duplicate create view complains`` () =
    expectError (Error.objectAlreadyExists "main.VUsers")
        """
            create view VUsers as
                select * from Users;
            create view VUsers as
                select * from USers;
        """

[<Test>]
let ``duplicate create table complains`` () =
    expectError (Error.objectAlreadyExists "main.Users")
        """
            create view Users as
                select * from Users;
        """

[<Test>]
let ``no such column to index`` () =
    expectError (Error.noSuchColumn "Goober")
        """
            create index IX_Users_Goober on Users(Id, Goober);
        """

[<Test>]
let ``no such column to index on creation`` () =
    expectError (Error.noSuchColumn "Qux")
        """
            create table MetaSyntactic
                ( Foo int
                , Bar int
                , Baz int
                , unique (Foo, Bar, Qux)
                );
        """

[<Test>]
let ``can't drop table referenced by others`` () =
    expectError (Error.tableIsReferencedByFKs "main.Users" ["main.UserGroupMaps"])
        "drop table Users"