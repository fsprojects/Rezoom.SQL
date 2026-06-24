module TypeProviderUser.SQLite.TestUserTypeAnnotations
// End-to-end TPU coverage for the Rezoom.SQL.Annotations attribute
// path. Compiles a real SQL<"..."> against the SQLite backend, then
// inspects the generated Command's Fragments to confirm the
// RawBackendSQLType value declared in TypeProviderUser.UserTypes
// (CompactInt = "MEDIUMINT") arrived in the emitted CAST.
open NUnit.Framework
open Rezoom.SQL
open Rezoom.SQL.Mapping

// No parameters, so no DB connection is needed to materialize this
// command and read its Fragments — the TP-generated factory builds a
// CommandData directly from the literal fragments it baked at
// compile time.
type CastToCompactInt = SQL<"select cast(42 as CompactInt) as c">

[<Test>]
let ``CompactInt RawBackendSQLType reaches CAST in generated command`` () =
    let cmd = CastToCompactInt.Command()
    let sql = CommandFragment.Stringize(cmd.Fragments)
    Assert.IsTrue
        ( sql.Contains("MEDIUMINT")
        , sprintf "Expected emitted SQL to contain 'MEDIUMINT' (the [<RawBackendSQLType>] on CompactInt). Got: %s" sql
        )
    // Belt-and-suspenders: the literal made it through verbatim,
    // not e.g. the default INT mapping for CompactInt's underlying int.
    Assert.IsFalse
        ( sql.Contains("INT NOT NULL")
        , sprintf "Did not expect the default INT mapping in emitted SQL. Got: %s" sql
        )
