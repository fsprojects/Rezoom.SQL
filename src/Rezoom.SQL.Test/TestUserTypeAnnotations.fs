module Rezoom.SQL.Test.TestUserTypeAnnotations
open NUnit.Framework
open Rezoom.SQL.Compiler

// --- SQLite: RawBackendSQLType emits the literal type verbatim --------

[<Test>]
let ``sqlite RawBackendSQLType emits the literal type in CREATE TABLE`` () =
    { sqliteTestWithUserTypes with
        Command = "create table T(x CompactInt)"
        Expect =
            { expect with
                OutputCommand =
                    """ CREATE TABLE "T" ( "x" MEDIUMINT NOT NULL ); """.SmushWhitespace()
                    |> Some
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite nullable RawBackendSQLType column keeps the literal type`` () =
    { sqliteTestWithUserTypes with
        Command = "create table T(x CompactInt null)"
        Expect =
            { expect with
                OutputCommand =
                    """ CREATE TABLE "T" ( "x" MEDIUMINT ); """.SmushWhitespace()
                    |> Some
            } |> Good
    } |> assertSimple

// SQLite's TypeName mapper produces `VARCHAR` for StringTypeName _
// regardless of length (it discards the length parameter), so the
// SQLTypeLength attribute is silently a no-op for SQLite emission.
// Still useful to assert that it doesn't blow up.
[<Test>]
let ``sqlite SQLTypeLength on a string DU is accepted without error`` () =
    { sqliteTestWithUserTypes with
        Command = "create table T(x ShortName)"
        Expect =
            { expect with
                OutputCommand =
                    """ CREATE TABLE "T" ( "x" VARCHAR NOT NULL ); """.SmushWhitespace()
                    |> Some
            } |> Good
    } |> assertSimple

// --- TSQL: SQLTypeLength flows through StringTypeName into NVARCHAR(N) -

[<Test>]
let ``tsql SQLTypeLength on a string DU emits NVARCHAR(N)`` () =
    { tsqlTestWithUserTypes with
        Command = "create table T(x ShortName)"
        Expect =
            { expect with
                OutputCommand =
                    """ CREATE TABLE [T] ( [x] NVARCHAR(80) NOT NULL ); """.SmushWhitespace()
                    |> Some
            } |> Good
    } |> assertSimple

[<Test>]
let ``tsql RawBackendSQLType emits the literal type in CREATE TABLE`` () =
    { tsqlTestWithUserTypes with
        Command = "create table T(x CompactInt)"
        Expect =
            { expect with
                OutputCommand =
                    """ CREATE TABLE [T] ( [x] MEDIUMINT NOT NULL ); """.SmushWhitespace()
                    |> Some
            } |> Good
    } |> assertSimple

[<Test>]
let ``without UserTypes a custom type name fails to resolve`` () =
    // Sanity check that the default (Empty) library still produces the
    // expected SQ067 when a test uses a name the library doesn't know.
    // Proves sqliteTest rig defaults to empty usertypes.
    { sqliteTest with
        Command = "create table T(x CompactInt)"
        Expect = BadCommand "SQ067: Type name ``CompactInt`` is not a built-in nor found in user assemblies ()"
    } |> assertSimple
