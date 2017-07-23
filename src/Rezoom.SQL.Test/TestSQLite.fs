module Rezoom.SQL.Test.TestSQLite
open NUnit.Framework
open Rezoom.SQL.Compiler

[<Test>]
let ``sqlite non-idempotent random`` () =
    { sqliteTest with
        Command = "select random() as r;"
        Expect =
            { expect with
                Idempotent = Some false
                ResultSets = Some [ [ "r", { Type = IntegerType Integer64; Nullable = false } ] ]
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite non-idempotent randomblob`` () =
    { sqliteTest with
        Command = "select randomblob(4) as r;"
        Expect =
            { expect with
                Idempotent = Some false
                ResultSets = Some [ [ "r", { Type = BinaryType; Nullable = false } ] ]
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite non-idempotent random in subquery`` () =
    { sqliteTest with
        Command = "select * from (select random() r) q;"
        Expect =
            { expect with
                Idempotent = Some false
                ResultSets = Some [ [ "r", { Type = IntegerType Integer64; Nullable = false } ] ]
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite custom constraint name`` () =
    { sqliteTest with
        Command = "create table X(a int constraint myname unique)"
        Expect =
            { expect with
                OutputCommand =
                    """
                    CREATE TABLE "X"  ( "a" INTEGER NOT NULL CONSTRAINT "myname" UNIQUE );
                    """.Trim() |> Some
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite custom table constraint name`` () =
    { sqliteTest with
        Command = "create table X(a int, constraint myname unique (a))"
        Expect =
            { expect with
                OutputCommand =
                    """
                    CREATE TABLE "X"  ( "a" INTEGER NOT NULL , CONSTRAINT "myname" UNIQUE("a" ASC) );
                    """.Trim() |> Some
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite dump function signatures`` () =
    for KeyValue(_, func) in sqliteTest.TestBackend.InitialModel.Builtin.Functions do
        printfn "%s" (dumpSignature func)
        printfn ""