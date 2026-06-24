module TypeProviderUser.Postgres.TestUserPrimitiveSystemObject
open NUnit.Framework
open Rezoom.SQL
open Rezoom.SQL.Raw
open TypeProviderUser.Postgres.UserTypes

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
let ``select roundtrips an Address user primitive over System.Object`` () =
    let results = InsertAndSelectAddresses.Command(homerAddr, margeAddr) |> runOnTestData
    Assert.AreEqual(2, results.Count)
    Assert.AreEqual(homerAddr, results.[0].Home)
    Assert.AreEqual(margeAddr, results.[1].Home)

type FindAddressByParameterEquality = SQL<"""
insert into UserAddresses(UserId, Home)
values((select Id from Users where Name = 'Homer'), @homer);
insert into UserAddresses(UserId, Home)
values((select Id from Users where Name = 'Marge'), @bart);
select Home from UserAddresses where Home = @needle;
""">

[<Test>]
let ``select with Address parameter equality matches via PG jsonb = operator`` () =
    let results =
        FindAddressByParameterEquality.Command(bartAddr, homerAddr, homerAddr)
        |> runOnTestData
    Assert.AreEqual(1, results.Count)
    Assert.AreEqual(homerAddr, results.[0].Home)

type FindAddressByStateViaJsonOperator = SQL<"""
insert into UserAddresses(UserId, Home)
values((select Id from Users where Name = 'Homer'), @homer);
insert into UserAddresses(UserId, Home)
values((select Id from Users where Name = 'Marge'), @marge);
select Home from UserAddresses ua where unsafe_inject_raw(@filter) order by ua.Id;
""">

[<Test>]
let ``PG jsonb path operator on Address column works via unsafe_inject_raw`` () =
    // We alias UserAddresses as ua in the SQL above so the raw filter
    // can reference the column with a known qualifier. Rezoom's PG
    // backend emits identifiers unquoted, so PG folds them to lowercase
    // — the raw filter must use ua.home (lowercase) to resolve.
    let results =
        FindAddressByStateViaJsonOperator.Command
            ( filter = [| sql "ua.home ->> 'City' = 'Shelbyville'" |]
            , homer = homerAddr
            , marge = bartAddr
            )
        |> runOnTestData
    Assert.AreEqual(1, results.Count)
    Assert.AreEqual(bartAddr, results.[0].Home)

type FindAddressByInList = SQL<"""
insert into UserAddresses(UserId, Home)
values((select Id from Users where Name = 'Homer'), @homer);
insert into UserAddresses(UserId, Home)
values((select Id from Users where Name = 'Marge'), @marge);
select Home from UserAddresses where Home in @needles;
""">

[<Test>]
let ``select Address where in non-empty list matches the expected row`` () =
    let results =
        FindAddressByInList.Command(homerAddr, bartAddr, [| homerAddr |])
        |> runOnTestData
    Assert.AreEqual(1, results.Count)
    Assert.AreEqual(homerAddr, results.[0].Home)

[<Test>]
let ``select Address where in empty list returns zero rows via jsonb empty-IN substitution`` () =
    let results =
        FindAddressByInList.Command(homerAddr, bartAddr, [||])
        |> runOnTestData
    Assert.AreEqual(0, results.Count)
