module Rezoom.SQL.Test.TestUserTypeAnnotations
open NUnit.Framework
open Rezoom.SQL.Mapping

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

// --- SQLParameterDbType loader-inspection regression tests ------------
// These do not exercise SQL emission. Instead they load the user-types
// library and inspect UserPrimitiveType.SQLParameterDbType directly,
// catching attribute-loader breakage at compiler-test speed instead of
// having to wait for a TPU run to surface it as a downstream PG error
// like `column "home" is of type jsonb but expression is of type text`.

let private userTypesLib =
    lazy ((userModelByName "user-model-7-usertypes").UserTypeLibrary)

let private primitive name =
    match userTypesLib.Value.UserPrimitiveByName(name) with
    | FoundType prim -> prim
    | AmbiguousType _ ->
        Assert.Fail(sprintf "User primitive '%s' is ambiguous in the loaded library." name)
        Unchecked.defaultof<_>
    | NotFoundType _ ->
        Assert.Fail(sprintf "User primitive '%s' was not found in the loaded library." name)
        Unchecked.defaultof<_>

[<Test>]
let ``SQLParameterDbType single-arg ctor on AnsiLabel loads as Some(DbType, int)`` () =
    // [<SQLParameterDbType(System.Data.DbType.AnsiString)>] on AnsiLabel
    // is the standard-DbType ctor; the C# attribute delegates to the
    // two-arg form with property name "DbType" and value (int)dbType,
    // and the loader records the same shape.
    let expected = Some ("DbType", int System.Data.DbType.AnsiString)
    Assert.That((primitive "AnsiLabel").SQLParameterDbType, Is.EqualTo(expected))

[<Test>]
let ``SQLParameterDbType two-arg ctor on OpaqueDbTypeProbe loads as Some(prop, int)`` () =
    // [<SQLParameterDbType("NpgsqlDbType", 36)>] on OpaqueDbTypeProbe is
    // the escape-hatch ctor used by provider-specific enums; the
    // attribute identity check, ctor-arity branch, and tuple read all
    // have to survive for the metadata to round-trip.
    let expected = Some ("NpgsqlDbType", 36)
    Assert.That((primitive "OpaqueDbTypeProbe").SQLParameterDbType, Is.EqualTo(expected))

[<Test>]
let ``SQLParameterDbType is None on a primitive without the attribute`` () =
    // CompactInt has [<RawBackendSQLType>] but no [<SQLParameterDbType>],
    // so the loader should leave the field as None. Guards against a
    // future change that accidentally always-Somes the field.
    Assert.That((primitive "CompactInt").SQLParameterDbType, Is.EqualTo(None))
