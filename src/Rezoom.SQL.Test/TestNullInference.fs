module Rezoom.SQL.Test.TestNullInference
open System
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Compiler
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
let ``coalesce forces all but last arg nullable`` () =
    expect
        """
            select coalesce(@a, @b, @c, @d) as c
        """
        [   "c", "<scalar>"
        ]
        [   for p in "abc" -> (string p, "<scalar>?")
            yield "d", "<scalar>"
        ]

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
    expect
        """
            select case when 1=0 then 1 end as c
        """
        [
            "c", "<numeric>?"
        ] []

[<Test>]
let ``insert into nullable column with parameter should be nullable`` () =
    expect
        """
            insert into Users(Id, Name, Email)
            values (@x, @y, '');
            select 0 as ignore;
        """
        [   "ignore", "<numeric>"
        ]
        [   "x", "INT"
            "y", "STRING?"
        ]

[<Test>]
let ``insert into nullable column with parameter from select should be nullable`` () =
    expect
        """
            insert into Users(Id, Name, Email)
                select @x, @y, '';

            select 0 as ignore;
        """
        [   "ignore", "<numeric>"
        ]
        [   "x", "INT"
            "y", "STRING?"
        ]

[<Test>]
let ``update into nullable column with parameter should be nullable`` () =
    expect
        """
            update Users
            set Id = @x
              , Name = @y
            where true;
            select 0 as ignore;
        """
        [   "ignore", "<numeric>"
        ]
        [   "x", "INT"
            "y", "STRING?"
        ]