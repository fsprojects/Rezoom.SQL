module Rezoom.SQL.Test.TestParserErrors
open System
open NUnit.Framework
open FsUnit
open Rezoom.SQL
open Rezoom.SQL.Mapping

[<Test>]
let ``invalid CTE`` () =
    expectError "SQ000: Expecting: whitespace or ')'"
        """
            with cte ( as
            select * from Users u
            )
            select * from cte
        """

[<Test>]
let ``small input`` () =
    expectError "SQ000: Expecting: end of input or whitespace" "s"

[<Test>]
let ``error at eof`` () =
    expectError
        "SQ000: Expecting: DISTINCT, MANY, ONE, OPTIONAL, TOP, expr, name, whitespace or '*'"
        "select"

[<Test>]
let ``select top not supported`` () =
    expectError
        ("SQ000: SELECT TOP (X) syntax is not supported, use LIMIT (X) at the end of your query"
        + Environment.NewLine
        + "instead")
        "select top 1 1 as x;"