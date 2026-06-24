module TypeProviderUser.TSQL.TestUserPrimitiveJson
open NUnit.Framework
open Rezoom.SQL
open TypeProviderUser.TSQL.UserTypes

// Same pattern as TypeProviderUser.Postgres.TestUserPrimitiveSystemObject:
// an obj-underlying user primitive whose value travels as a JSON string.
// On TSQL the backing column is the SQL Server 2025 `json` type, which
// is the natural counterpart to PG's `jsonb`.

let private homerAddr =
    {   Street = "742 Evergreen Terrace"
        City = "Springfield"
        State = "OR"
        Zip = "97477"
    }

let private margeAddr =
    {   Street = "742 Evergreen Terrace"
        City = "Springfield"
        State = "OR"
        Zip = "97477"
    }

let private bartAddr =
    {   Street = "1313 Mockingbird Lane"
        City = "Shelbyville"
        State = "OR"
        Zip = "97001"
    }

type InsertAndSelectAddresses = SQL<"""
insert into UserAddresses(UserId, Home)
values((select Id from Users where Name = 'Homer'), @homer);
insert into UserAddresses(UserId, Home)
values((select Id from Users where Name = 'Marge'), @marge);
select Home from UserAddresses order by Id;
""">

[<Test>]
let ``select roundtrips an Address user primitive over TSQL json`` () =
    let results = InsertAndSelectAddresses.Command(homerAddr, margeAddr) |> runOnTestData
    Assert.AreEqual(2, results.Count)
    Assert.AreEqual(homerAddr, results.[0].Home)
    Assert.AreEqual(margeAddr, results.[1].Home)

// SQL Server's `json` type has no `=` operator (SQL Server raises "The
// JSON data type cannot be compared or sorted, except when using the
// IS NULL operator"), mirroring PG's lack of `=` for `point`. We test
// parameter equality the recommended way: vendor body runs TSQL-native
// SQL using JSON_VALUE on a known field, IMAGINE clause informs the
// typechecker of the parameter types and result shape.
type FindAddressByJsonValueVendor = SQL<"""
insert into UserAddresses(UserId, Home)
values((select Id from Users where Name = 'Homer'), @homer);
insert into UserAddresses(UserId, Home)
values((select Id from Users where Name = 'Marge'), @bart);
vendor tsql {
    select Home from UserAddresses where JSON_VALUE(Home, '$.City') = {@city}
} imagine {
    select Home from UserAddresses where @city = ''
};
""">

[<Test>]
let ``select Address by JSON_VALUE matches via vendor/imagine`` () =
    // @city stays typed as string in Rezoom's view; the vendor body
    // uses TSQL's JSON_VALUE function on the json column to compare a
    // specific field. The Home parameters (@homer, @bart) are typed
    // Address and exercise the user-type → nvarchar pipeline on the
    // INSERT side.
    let results =
        FindAddressByJsonValueVendor.Command
            ( bart = bartAddr
            , city = "Shelbyville"
            , homer = homerAddr
            )
        |> runOnTestData
    Assert.AreEqual(1, results.Count)
    Assert.AreEqual(bartAddr, results.[0].Home)
