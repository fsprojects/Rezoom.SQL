namespace StaticQL.Test
open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open StaticQL
open StaticQL.Mapping

[<TestClass>]
type TestRoundTrip() =
    let roundtrip (sql : string) =
        let userModel = UserModel.Load("../../user-model-1", ".")
        let parsed = CommandEffect.OfSQL(userModel.Model, "anonymous", sql)
        let indexer = { new IParameterIndexer with member __.ParameterIndex(_) = 0 }
        let fragments = userModel.Backend.ToCommandFragments(indexer, parsed.Statements)
        let str = CommandFragment.Stringize(fragments)
        Console.WriteLine(str)
        let parsedBack = CommandEffect.OfSQL(userModel.Model, "anonymous", str)
        let fragmentsBack = userModel.Backend.ToCommandFragments(indexer, parsedBack.Statements)
        let strBack = CommandFragment.Stringize(fragmentsBack)
        Console.WriteLine(String('-', 80))
        Console.WriteLine(strBack)
        Assert.AreEqual(str, strBack)

    [<TestMethod>]
    member __.TestSelect() =
        roundtrip """
            select * from Users u where u.Id = 1
        """
    [<TestMethod>]
    member __.TestFancySelect() =
        roundtrip """
            select g.*, u.*
            from Users u
            left join UserGroupMaps gm on gm.UserId = u.Id
            left join Groups g on g.Id = gm.GroupId
            where g.Name like '%grp%' escape '%'
        """