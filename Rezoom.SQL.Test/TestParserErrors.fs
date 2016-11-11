module Rezoom.SQL.Test.TestParserErrors
open System
open NUnit.Framework
open FsUnit
open Rezoom.SQL
open Rezoom.SQL.Mapping

[<Test>]
let ``invalid CTE`` () =
    expectError "Expecting: end of input, whitespace, '--' or '/*'\r\n"
        """
            with cte ( as
            select * from Users u
            )
            select * from cte
        """