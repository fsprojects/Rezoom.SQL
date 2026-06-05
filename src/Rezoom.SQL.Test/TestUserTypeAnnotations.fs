module Rezoom.SQL.Test.TestUserTypeAnnotations
open NUnit.Framework

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

// --- CAST exprs go through the same TypeName mapper -------------------

[<Test>]
let ``sqlite CAST to RawBackendSQLType emits literal in expression context`` () =
    { sqliteTestWithUserTypes with
        Command = "select cast(42 as CompactInt) as c"
        Expect =
            { expect with
                OutputCommand =
                    """ SELECT CAST(42 AS MEDIUMINT) AS "c"; """.SmushWhitespace()
                    |> Some
            } |> Good
    } |> assertSimple

[<Test>]
let ``tsql CAST to RawBackendSQLType emits literal in expression context`` () =
    { tsqlTestWithUserTypes with
        Command = "select cast(42 as CompactInt) as c"
        Expect =
            { expect with
                OutputCommand =
                    """ SELECT CAST(42 AS MEDIUMINT) AS [c]; """.SmushWhitespace()
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

// checking that we can override built-in types with usertypes

[<Test>]
let ``sqlite override Guid via user-types extension emits the override's RawBackendSQLType`` () =
    // Rezoom.SQL.Test.UserTypes.GuidOverrideExtension carries
    // [<RawBackendSQLType("CHAR(36)")>] on ToPrimitive.
    { sqliteTestWithUserTypes with
        Command = "create table T(x guid)"
        Expect =
            { expect with
                OutputCommand =
                    """ CREATE TABLE "T" ( "x" CHAR(36) NOT NULL ); """.SmushWhitespace()
                    |> Some
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite Guid override applies regardless of source-text casing (uppercase)`` () =
    // the parser is case-insensitive for keyword recognition, so an
    // override on Guid applies to every casing, which is different from usual UserTypes that require exact case match.
    { sqliteTestWithUserTypes with
        Command = "create table T(x GUID)"
        Expect =
            { expect with
                OutputCommand =
                    """ CREATE TABLE "T" ( "x" CHAR(36) NOT NULL ); """.SmushWhitespace()
                    |> Some
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite Guid override applies regardless of source-text casing (Pascal)`` () =
    { sqliteTestWithUserTypes with
        Command = "create table T(x Guid)"
        Expect =
            { expect with
                OutputCommand =
                    """ CREATE TABLE "T" ( "x" CHAR(36) NOT NULL ); """.SmushWhitespace()
                    |> Some
            } |> Good
    } |> assertSimple

[<Test>]
let ``tsql Guid override emits the RawBackendSQLType literal verbatim`` () =
    { tsqlTestWithUserTypes with
        Command = "create table T(x guid)"
        Expect =
            { expect with
                OutputCommand =
                    """ CREATE TABLE [T] ( [x] CHAR(36) NOT NULL ); """.SmushWhitespace()
                    |> Some
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite DateTimeOffset extension override emits via underlying string`` () =
    // DateTimeOffset is normally unsupported in SQLite at the backend level, but if overridden, can be used
    { sqliteTestWithUserTypes with
        Command = "create table T(x datetimeoffset)"
        Expect =
            { expect with
                OutputCommand =
                    """ CREATE TABLE "T" ( "x" VARCHAR NOT NULL ); """.SmushWhitespace()
                    |> Some
            } |> Good
    } |> assertSimple

[<Test>]
let ``without UserTypes the guid keyword still emits the parser default`` () =
    // without usertypes library, the parser's GuidTypeName path is unchanged
    // and SQLite emits its default backend BLOB mapping.
    { sqliteTest with
        Command = "create table T(x guid)"
        Expect =
            { expect with
                OutputCommand =
                    """ CREATE TABLE "T" ( "x" BLOB NOT NULL ); """.SmushWhitespace()
                    |> Some
            } |> Good
    } |> assertSimple

// checking that UserPrimitives can be mapped to/from byte array

[<Test>]
let ``sqlite DU over byte[] emits BLOB`` () =
    { sqliteTestWithUserTypes with
        Command = "create table T(x FileHash)"
        Expect =
            { expect with
                OutputCommand =
                    """ CREATE TABLE "T" ( "x" BLOB NOT NULL ); """.SmushWhitespace()
                    |> Some
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite nullable DU over byte[] emits BLOB without NOT NULL`` () =
    { sqliteTestWithUserTypes with
        Command = "create table T(x FileHash null)"
        Expect =
            { expect with
                OutputCommand =
                    """ CREATE TABLE "T" ( "x" BLOB ); """.SmushWhitespace()
                    |> Some
            } |> Good
    } |> assertSimple

[<Test>]
let ``tsql DU over byte[] with SQLTypeLength emits VARBINARY(N)`` () =
    // ShortHash carries [<SQLTypeLength(32)>]. The TSQL mapper consumes
    // the length parameter the same way it does for StringTypeName.
    { tsqlTestWithUserTypes with
        Command = "create table T(x ShortHash)"
        Expect =
            { expect with
                OutputCommand =
                    """ CREATE TABLE [T] ( [x] VARBINARY(32) NOT NULL ); """.SmushWhitespace()
                    |> Some
            } |> Good
    } |> assertSimple

[<Test>]
let ``sqlite DU over byte[] with SQLTypeLength still emits BLOB`` () =
    // SQLite's BinaryTypeName(_) "BLOB" discards the length parameter
    { sqliteTestWithUserTypes with
        Command = "create table T(x ShortHash)"
        Expect =
            { expect with
                OutputCommand =
                    """ CREATE TABLE "T" ( "x" BLOB NOT NULL ); """.SmushWhitespace()
                    |> Some
            } |> Good
    } |> assertSimple

[<Test>]
let ``tsql DU over byte[] without SQLTypeLength emits VARBINARY(max)`` () =
    { tsqlTestWithUserTypes with
        Command = "create table T(x FileHash)"
        Expect =
            { expect with
                OutputCommand =
                    """ CREATE TABLE [T] ( [x] VARBINARY(max) NOT NULL ); """.SmushWhitespace()
                    |> Some
            } |> Good
    } |> assertSimple
