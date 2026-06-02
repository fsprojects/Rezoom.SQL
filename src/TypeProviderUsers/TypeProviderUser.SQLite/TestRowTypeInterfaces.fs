module TypeProviderUser.SQLite.TestRowTypeInterfaces
// Happy-path tests for the SELECT<...> row-type interface implementation
// feature. Each test verifies a generated row type can be upcast to the
// declared interface(s) and that the values read through the interface
// reference match the underlying row.
open System
open System.Collections.Generic
open NUnit.Framework
open Rezoom.SQL
open TypeProviderUser.UserTypes

// --- Flat, all-primitive interface --------------------------------------

type SelectUserSummary = SQL<"""
select<IUserSummary> Name, Email from Users where Name = 'Homer'
""">

[<Test>]
let ``flat row implements interface with primitive properties`` () =
    let results = SelectUserSummary.Command() |> runOnTestData
    Assert.AreEqual(1, results.Count)
    let summary : IUserSummary = results.[0] :> IUserSummary
    Assert.AreEqual("Homer", summary.Name)
    Assert.AreEqual("homer.simpson@springfieldnuclear.com", summary.Email)

// --- User-primitive property type (UserId is a single-case DU
//     from TypeProviderUser.UserTypes) ----------------------------------

type SelectHasUserId = SQL<"""
select<IHasUserId> Id from Users where Name = 'Homer'
""">

[<Test>]
let ``row implements interface whose property is a user-primitive type`` () =
    // Single-column row so IScalar is also added — verify the row satisfies
    // both the user-declared interface AND the auto-IScalar without
    // interference.
    let results = SelectHasUserId.Command() |> runOnTestData
    Assert.AreEqual(1, results.Count)
    let row = results.[0]
    let withId : IHasUserId = row :> IHasUserId
    Assert.AreEqual(UserId 1L, withId.Id)
    let scalar : Rezoom.SQL.IScalar<UserId> = row :> _
    Assert.AreEqual(UserId 1L, scalar.ScalarValue)

// --- Multiple interfaces declared on one query --------------------------

type SelectMultiInterfaces = SQL<"""
select<IUserSummary, IHasUserId> Id, Name, Email from Users where Name = 'Homer'
""">

[<Test>]
let ``row implements multiple interfaces declared at once`` () =
    let results = SelectMultiInterfaces.Command() |> runOnTestData
    Assert.AreEqual(1, results.Count)
    let row = results.[0]
    let summary : IUserSummary = row :> IUserSummary
    let withId : IHasUserId = row :> IHasUserId
    Assert.AreEqual("Homer", summary.Name)
    Assert.AreEqual("homer.simpson@springfieldnuclear.com", summary.Email)
    Assert.AreEqual(UserId 1L, withId.Id)

// --- Nullable column projected through an F#-style option ---------------

type SelectHasBedtime = SQL<"""
update Users set BedtimeIfAny = @bedtime where Name = 'Homer';
select<IHasBedtime> Name, BedtimeIfAny from Users order by Name
""">

[<Test>]
let ``row implements interface with TimeOnly option property`` () =
    let bedtime = TimeOnly(22, 30, 0)
    let results = SelectHasBedtime.Command(Some bedtime) |> runOnTestData
    Assert.AreEqual(2, results.Count)
    let homer = (results |> Seq.find (fun r -> r.Name = "Homer")) :> IHasBedtime
    let marge = (results |> Seq.find (fun r -> r.Name = "Marge")) :> IHasBedtime
    Assert.AreEqual(Some bedtime, homer.BedtimeIfAny)
    Assert.AreEqual(None, marge.BedtimeIfAny)

// --- MANY navigation with IReadOnlyList<T> in the interface -------------

type SelectUserWithArticles = SQL<"""
select<IUserWithArticles>
    u.*,
    many Articles(a.Id, a.ArticleTitle, a.ArticleText)
from Users u
left join Articles a on a.AuthorId = u.Id
order by u.Name
""">

[<Test>]
let ``row implements interface with MANY navigation (IReadOnlyList)`` () =
    let results = SelectUserWithArticles.Command() |> runOnTestData
    Assert.AreEqual(2, results.Count)
    let homer = (results |> Seq.find (fun r -> r.Name = "Homer")) :> IUserWithArticles
    let marge = (results |> Seq.find (fun r -> r.Name = "Marge")) :> IUserWithArticles
    Assert.AreEqual("Homer", homer.Name)
    Assert.AreEqual(2, homer.Articles.Count)
    Assert.AreEqual(0, marge.Articles.Count)
    let titles =
        homer.Articles
        |> Seq.map (fun a -> a.ArticleTitle)
        |> Seq.sort
        |> Seq.toList
    Assert.AreEqual
        ( [ "My first review as a food critic."
            "My second review as a food critic." ]
        , titles )

// --- MANY navigation where the interface declares IEnumerable<T> -------
// Locks in the covariant cross-interface upcast: row has
// IReadOnlyList<ArticleRow>, interface wants IEnumerable<IArticleHeader>.

type SelectUserWithArticlesEnumerable = SQL<"""
select<IUserWithArticlesEnumerable>
    u.*,
    many Articles(a.Id, a.ArticleTitle, a.ArticleText)
from Users u
left join Articles a on a.AuthorId = u.Id
where u.Name = 'Homer'
""">

[<Test>]
let ``MANY navigation projects through interface declaring IEnumerable<T>`` () =
    let results = SelectUserWithArticlesEnumerable.Command() |> runOnTestData
    Assert.AreEqual(1, results.Count)
    let homer : IUserWithArticlesEnumerable = results.[0] :> _
    Assert.AreEqual("Homer", homer.Name)
    let titles =
        homer.Articles
        |> Seq.map (fun a -> a.ArticleTitle)
        |> Seq.sort
        |> Seq.toList
    Assert.AreEqual(2, titles.Length)

// --- ONE navigation -----------------------------------------------------

type SelectArticleWithAuthor = SQL<"""
select<IArticleWithAuthor>
    a.*,
    one Author(u.*)
from Articles a
join Users u on u.Id = a.AuthorId
order by a.Id
""">

[<Test>]
let ``row implements interface with ONE navigation`` () =
    let results = SelectArticleWithAuthor.Command() |> runOnTestData
    Assert.AreEqual(2, results.Count)
    let first = results.[0] :> IArticleWithAuthor
    Assert.AreEqual("My first review as a food critic.", first.ArticleTitle)
    // The nested sub-row was generated with IUserSummary attached as well,
    // so reaching through the interface gives interface-typed values.
    Assert.AreEqual("Homer", first.Author.Name)
    Assert.AreEqual("homer.simpson@springfieldnuclear.com", first.Author.Email)

// --- OPTIONAL navigation (F# option<T>) --------------------------------

type SelectUserWithMaybePicture = SQL<"""
update Users set ProfilePictureSHA256 = null where Name = 'Marge';
select<IUserWithMaybePicture>
    u.*,
    optional Picture(p.*)
from Users u
left join Pictures p on p.SHA256 = u.ProfilePictureSHA256
order by u.Name
""">

[<Test>]
let ``row implements interface with OPTIONAL navigation (option<T>)`` () =
    let results = SelectUserWithMaybePicture.Command() |> runOnTestData
    Assert.AreEqual(2, results.Count)
    let homer = (results |> Seq.find (fun r -> r.Name = "Homer")) :> IUserWithMaybePicture
    let marge = (results |> Seq.find (fun r -> r.Name = "Marge")) :> IUserWithMaybePicture
    Assert.IsTrue(homer.Picture.IsSome)
    Assert.IsTrue(marge.Picture.IsNone)
    let homerHash = homer.Picture.Value.SHA256
    Assert.AreEqual(32, homerHash.Length)
    for b in homerHash do Assert.AreEqual(0xffuy, b)
