module TypeProviderUser.Postgres.TestUserPrimitivePoint
open NUnit.Framework
open Rezoom.SQL
open Rezoom.SQL.Raw
open TypeProviderUser.Postgres.UserTypes

// Point2D maps to PG's `point` type via the System.Object escape hatch
// with NpgsqlPoint as the driver value (not a string), exercising a
// different shape from the jsonb/Address case in TestUserPrimitiveSystemObject.

let private homerPoint = { X = 1.5; Y = 2.5 }
let private margePoint = { X = 1.5; Y = 2.5 }
let private bartPoint = { X = -7.25; Y = 99.0 }

type InsertAndSelectPoints = SQL<"""
insert into UserLocations(UserId, Coord)
values((select Id from Users where Name = 'Homer'), @homer);
insert into UserLocations(UserId, Coord)
values((select Id from Users where Name = 'Marge'), @marge);
select Coord from UserLocations order by Id;
""">

[<Test>]
let ``select roundtrips a Point2D user primitive over PG point`` () =
    let results = InsertAndSelectPoints.Command(homerPoint, margePoint) |> runOnTestData
    Assert.AreEqual(2, results.Count)
    Assert.AreEqual(homerPoint, results.[0].Coord)
    Assert.AreEqual(margePoint, results.[1].Coord)

// PG's point type has no `=` operator (42883: "operator does not
// exist: point = point"). Equality is `~=` (the same-as operator),
// which Rezoom's parser doesn't know — unsafe_inject_raw is the
// idiomatic escape hatch here. The parameter @needle still binds
// through Rezoom as a Point2D, then PG's ~= compares it against the
// column value at row scan time, exercising the full
// parameter-as-point pipeline.
type FindPointByParameterSameAs = SQL<"""
insert into UserLocations(UserId, Coord)
values((select Id from Users where Name = 'Homer'), @homer);
insert into UserLocations(UserId, Coord)
values((select Id from Users where Name = 'Marge'), @bart);
select Coord from UserLocations ul where unsafe_inject_raw(@filter);
""">

[<Test>]
let ``select with Point2D parameter equality matches via PG ~= operator`` () =
    // Identifiers are emitted unquoted by Rezoom's PG backend, so PG
    // folds them lowercase — `ul.coord`, not `"Coord"`.
    //
    // Caveat: Rezoom.SQL.Raw.arg does not apply user-type ToPrimitive
    // translation — it routes the value straight to ADO.NET with a
    // guessed DbType. So we cannot pass a Point2D here and expect the
    // Point2D → NpgsqlPoint conversion to happen automatically. We
    // pre-convert to NpgsqlPoint in user space; Npgsql then
    // auto-detects the wire format from the value's runtime type.
    // The fully-translated user-type → parameter pipeline is already
    // exercised by the INSERT in the roundtrip test above; this test
    // covers the WHERE-side parameter comparison via ~=.
    let needle = NpgsqlTypes.NpgsqlPoint(homerPoint.X, homerPoint.Y)
    let results =
        FindPointByParameterSameAs.Command
            ( bart = bartPoint
            , filter = [| sql "ul.coord ~= "; arg needle |]
            , homer = homerPoint
            )
        |> runOnTestData
    Assert.AreEqual(1, results.Count)
    Assert.AreEqual(homerPoint, results.[0].Coord)

// Same functional intent as FindPointByParameterSameAs above, but using
// the vendor/imagine escape hatch instead of unsafe_inject_raw. The
// IMAGINE clause is typechecked against Rezoom's dialect, informing the
// typechecker that @needle is a Point2D and that the result set has a
// Coord column. The vendor body runs PG-native SQL — including ~= and
// the `{@needle}` extra-brace param reference — and the user-type
// translation pipeline still fires for @needle on the parameter side,
// so the caller passes a real Point2D, not a NpgsqlPoint, from F#.
type FindPointByParameterVendor = SQL<"""
insert into UserLocations(UserId, Coord)
values((select Id from Users where Name = 'Homer'), @homer);
insert into UserLocations(UserId, Coord)
values((select Id from Users where Name = 'Marge'), @bart);
vendor postgres {
    select Coord from UserLocations where coord ~= {@needle}
} imagine {
    select Coord from UserLocations where Coord = @needle
};
""">

[<Test>]
let ``select with Point2D parameter equality matches via vendor ~= with IMAGINE`` () =
    // No manual NpgsqlPoint conversion: @needle stays typed as Point2D
    // all the way through Rezoom, so the user-type SQLParameterDbType
    // attribute is applied to the actual parameter being compared.
    let results =
        FindPointByParameterVendor.Command
            ( bart = bartPoint
            , homer = homerPoint
            , needle = homerPoint
            )
        |> runOnTestData
    Assert.AreEqual(1, results.Count)
    Assert.AreEqual(homerPoint, results.[0].Coord)
