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
        ("SQ000: Expecting: DISTINCT, MANY, ONE, OPTIONAL, expr, name, whitespace or '*'")
        "select"