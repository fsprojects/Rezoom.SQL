module Rezoom.SQL.Test.TestModelErrors
open System
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Compiler
open Rezoom.SQL.Mapping

[<Test>]
let ``duplicate create view complains`` () =
    expectError (Error.objectAlreadyExists "VUsers")
        """
            create view VUsers as
                select * from Users;
            create view VUsers as
                select * from USers;
        """

[<Test>]
let ``duplicate create table complains`` () =
    expectError (Error.objectAlreadyExists "Users")
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
