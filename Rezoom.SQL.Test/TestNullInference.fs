module Rezoom.SQL.Test.TestNullInference
open System
open NUnit.Framework
open FsUnit
open Rezoom.SQL
open Rezoom.SQL.Mapping

let expect (sql : string) expectedColumns expectedParams =
    let userModel = userModel1()
    let parsed = CommandEffect.OfSQL(userModel.Model, "anonymous", sql)
    let sets = parsed.ResultSets() |> Seq.toArray
    if sets.Length <> 1 then failwith "expected 1 result set"
    let cols = sets.[0].Columns |> Seq.map (fun c -> c.ColumnName.Value, c.Expr.Info.Type.ToString()) |> Seq.toList
    printfn "%A" cols
    Assert.AreEqual(expectedColumns, cols)
    let pars =
        parsed.Parameters
        |> Seq.map (fun (NamedParameter name, ty) -> name.Value, ty.ToString())
        |> Seq.toList
    printfn "%A" pars
    Assert.AreEqual(expectedParams, pars)

[<Test>]
let ``coalesce(a + b, 1)`` () =
    expect
        """
            select coalesce(@a + @b, 1) as c
        """
        [   "c", "<numeric>"
        ]
        [   "a", "<numeric>?"
            "b", "<numeric>?"
        ]

[<Test>]
let ``coalesce(a + b, null)`` () =
    expect
        """
            select coalesce(@a + @b, null) as c
        """
        [   "c", "<numeric>?"
        ]
        [   "a", "<numeric>?"
            "b", "<numeric>?"
        ]

[<Test>]
let ``coalesce(nullable(a) + b, 1)`` () =
    expect
        """
            select coalesce(nullable(@a) + @b, 1) as c
        """
        [   "c", "<numeric>"
        ]
        [   "a", "<numeric>?"
            "b", "<numeric>"
        ]

[<Test>]
let ``coalesce(a + nullable(b), 1)`` () =
    expect
        """
            select coalesce(@a + nullable(@b), 1) as c
        """
        [   "c", "<numeric>"
        ]
        [   "a", "<numeric>"
            "b", "<numeric>?"
        ]

[<Test>]
let ``case nullable`` () =
    expect
        """
            select case when 1=1 then 1 else null end as c
        """
        [    "c", "<numeric>?"
        ] []

[<Test>]
let ``case not nullable`` () =
    expect
        """
            select case when null then 1 else 0 end as c
        """
        [   "c", "<numeric>"
        ] []

[<Test>]
let ``case not handled means null`` () =
    // TODO: TSQL backend should deny this syntax altogether
    expect
        """
            select case when 1=0 then 1 end as c
        """
        [
            "c", "<numeric>?"
        ] []