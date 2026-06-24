module TypeProviderUser.TSQL.TestUserPrimitiveGeography
open NUnit.Framework
open Rezoom.SQL
open TypeProviderUser.TSQL.UserTypes

// Same pattern as TypeProviderUser.Postgres.TestUserPrimitivePoint:
// an obj-underlying user primitive that maps to a SQL Server backend
// type with no `=` operator (geography). Parameter equality goes
// through vendor + IMAGINE using TSQL's `.STEquals(other) = 1`
// method-call style.

let private homerLoc = { Latitude = 44.0521; Longitude = -123.0868 }
let private margeLoc = { Latitude = 44.0521; Longitude = -123.0868 }
let private bartLoc  = { Latitude = 47.6062; Longitude = -122.3321 }

type InsertAndSelectLocations = SQL<"""
insert into UserLocations(UserId, Coord)
values((select Id from Users where Name = 'Homer'), @homer);
insert into UserLocations(UserId, Coord)
values((select Id from Users where Name = 'Marge'), @marge);
select Coord from UserLocations order by Id;
""">

[<Test>]
let ``select roundtrips a GeoLocation user primitive over TSQL geography`` () =
    let results = InsertAndSelectLocations.Command(homerLoc, margeLoc) |> runOnTestData
    Assert.AreEqual(2, results.Count)
    Assert.AreEqual(homerLoc, results.[0].Coord)
    Assert.AreEqual(margeLoc, results.[1].Coord)

// `geography = geography` raises "Invalid operator for data type" in
// SQL Server. The canonical equality check is `.STEquals(other) = 1`.
// Rezoom's parser doesn't know method-call syntax on UDT columns, so
// we use vendor/imagine the same way the PG Point2D parameter test
// does: vendor body runs the TSQL method call, IMAGINE typechecks
// the parameter and result shape.
//
// On the parameter binding: the runtime applies the GeoLocation user
// type's SQLParameterDbType (NVarChar), so @needle is sent as the WKT
// nvarchar that ToPrimitive produces. SQL Server's STEquals takes a
// geography on both sides; the parameter's nvarchar value is
// implicitly converted to geography in the comparison context
// (geography has higher data-type precedence and STEquals' parameter
// is typed geography).
type FindLocationByStEqualsVendor = SQL<"""
insert into UserLocations(UserId, Coord)
values((select Id from Users where Name = 'Homer'), @homer);
insert into UserLocations(UserId, Coord)
values((select Id from Users where Name = 'Marge'), @bart);
vendor tsql {
    select Coord from UserLocations
    where Coord.STEquals(geography::STGeomFromText({@needle}, 4326)) = 1
} imagine {
    select Coord from UserLocations where @needle = ''
};
""">

[<Test>]
let ``select GeoLocation parameter equality matches via vendor STEquals`` () =
    // @needle is typed as string in IMAGINE because we're explicitly
    // building the geography from WKT inside the vendor body — this
    // exercises that the typechecker can still propagate the result-
    // set column type (Coord : GeoLocation) from the IMAGINE clause
    // even when the parameter type is something simpler. The @homer
    // and @bart INSERTs already cover the GeoLocation parameter
    // pipeline end-to-end.
    let results =
        FindLocationByStEqualsVendor.Command
            ( bart = bartLoc
            , homer = homerLoc
            , needle =
                System.String.Format
                    ( System.Globalization.CultureInfo.InvariantCulture
                    , "POINT({0} {1})", homerLoc.Longitude, homerLoc.Latitude )
            )
        |> runOnTestData
    Assert.AreEqual(1, results.Count)
    Assert.AreEqual(homerLoc, results.[0].Coord)

// Bonus: pass @needle as a real GeoLocation user-type parameter, fully
// preserving type-safety from F# all the way through to TSQL's
// STEquals. Mirrors the second Postgres Point2D vendor test (the one
// where the user-type parameter pipeline is fully engaged on both
// the INSERT and the WHERE side).
type FindLocationByGeoLocationVendor = SQL<"""
insert into UserLocations(UserId, Coord)
values((select Id from Users where Name = 'Homer'), @homer);
insert into UserLocations(UserId, Coord)
values((select Id from Users where Name = 'Marge'), @bart);
vendor tsql {
    select Coord from UserLocations
    where Coord.STEquals(geography::STGeomFromText(cast({@needle} as nvarchar(max)), 4326)) = 1
} imagine {
    select Coord from UserLocations where Coord = @needle
};
""">

[<Test>]
let ``select GeoLocation parameter equality matches via vendor STEquals with typed needle`` () =
    // The IMAGINE clause types @needle as GeoLocation (column = param),
    // so the F# caller passes a real GeoLocation. The vendor body
    // casts the bound nvarchar back to nvarchar(max) for safety, then
    // STGeomFromText. Demonstrates that vendor/imagine keeps the
    // user-type parameter pipeline intact even for backend operators
    // Rezoom can't parse.
    let results =
        FindLocationByGeoLocationVendor.Command
            ( bart = bartLoc
            , homer = homerLoc
            , needle = homerLoc
            )
        |> runOnTestData
    Assert.AreEqual(1, results.Count)
    Assert.AreEqual(homerLoc, results.[0].Coord)
