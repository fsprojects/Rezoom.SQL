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