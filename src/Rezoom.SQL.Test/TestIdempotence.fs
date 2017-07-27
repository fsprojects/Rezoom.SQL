module Rezoom.SQL.Test.Idempotence
open NUnit.Framework

[<Test>]
let ``control idempotent IN statement`` () =
    { sqliteTest with
        Command = "select 1 x where 1 in (select 1 x)"
        Expect =
            { expect with Idempotent = Some true } |> Good
    } |> assertSimple

[<Test>]
let ``left side of IN affects idempotence`` () =
    { sqliteTest with
        Command = "select 1 x where random() in (select 1 x)"
        Expect =
            { expect with Idempotent = Some false } |> Good
    } |> assertSimple

[<Test>]
let ``right side of IN affects idempotence`` () =
    { sqliteTest with
        Command = "select 1 x where 1 in (select random() x)"
        Expect =
            { expect with Idempotent = Some false } |> Good
    } |> assertSimple

[<Test>]
let ``group by clause does not inherently block idempotencee`` () =
    { sqliteTest with
        Command = "select x from (select 1 x) q group by x"
        Expect =
            { expect with Idempotent = Some true } |> Good
    } |> assertSimple

[<Test>]
let ``group by clause affects idempotence`` () =
    { sqliteTest with
        Command = "select random() x from (select 1 x) q group by random()"
        Expect =
            { expect with Idempotent = Some false } |> Good
    } |> assertSimple

[<Test>]
let ``having clause does not inherently block idempotencee`` () =
    { sqliteTest with
        Command = "select x from (select 1 x) q group by x having true"
        Expect =
            { expect with Idempotent = Some true } |> Good
    } |> assertSimple

[<Test>]
let ``having clause affects idempotence`` () =
    { sqliteTest with
        Command = "select x from (select 1 x) q group by x having random() > 0"
        Expect =
            { expect with Idempotent = Some false } |> Good
    } |> assertSimple

[<Test>]
let ``order by clause does not inherently block idempotence`` () =
    { sqliteTest with
        Command = "select 1 x order by 'stuff'"
        Expect =
            { expect with Idempotent = Some true } |> Good
    } |> assertSimple

[<Test>]
let ``order by clause affects idempotence`` () =
    { sqliteTest with
        Command = "select 1 x order by random()"
        Expect =
            { expect with Idempotent = Some false } |> Good
    } |> assertSimple

[<Test>]
let ``limt clause does not inherently block idempotence`` () =
    { sqliteTest with
        Command = "select 1 x order by 'stuff' limit 1"
        Expect =
            { expect with Idempotent = Some true } |> Good
    } |> assertSimple

[<Test>]
let ``limit clause affects idempotence`` () =
    { sqliteTest with
        Command = "select 1 x order by 'stuff' limit random()"
        Expect =
            { expect with Idempotent = Some false } |> Good
    } |> assertSimple

[<Test>]
let ``offset clause does not inherently block idempotence`` () =
    { sqliteTest with
        Command = "select 1 x order by 'stuff' limit 1 offset 1"
        Expect =
            { expect with Idempotent = Some true } |> Good
    } |> assertSimple

[<Test>]
let ``offset clause affects idempotence`` () =
    { sqliteTest with
        Command = "select 1 x order by 'stuff' limit 1 offset random()"
        Expect =
            { expect with Idempotent = Some false } |> Good
    } |> assertSimple