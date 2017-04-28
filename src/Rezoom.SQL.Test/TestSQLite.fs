module Rezoom.SQL.Test.TestSQLite
open NUnit.Framework
open Rezoom.SQL.Compiler

[<Test>]
let ``sqlite non-idempotent random`` () =
    { sqliteTest with
        Migration = ""
        Command = "select random() as r;"
        Expect =
            {   expect with
                    Idempotent = Some false
                    ResultSets = Some [ [ "r", { Type = IntegerType Integer64; Nullable = false } ] ];
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite non-idempotent randomblob`` () =
    { sqliteTest with
        Migration = ""
        Command = "select randomblob() as r;"
        Expect =
            {   expect with
                    Idempotent = Some false
                    ResultSets = Some [ [ "r", { Type = BinaryType; Nullable = false } ] ];
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite non-idempotent random in subquery`` () =
    { sqliteTest with
        Migration = ""
        Command = "select * from (select random() r) q;"
        Expect =
            {   expect with
                    Idempotent = Some false
                    ResultSets = Some [ [ "r", { Type = IntegerType Integer64; Nullable = false } ] ];
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite dump function signatures`` () =
    for KeyValue(_, func) in sqliteTest.TestBackend.InitialModel.Builtin.Functions do
        printfn "%s" (dumpSignature func)
        printfn ""