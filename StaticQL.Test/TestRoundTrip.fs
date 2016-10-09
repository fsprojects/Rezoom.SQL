namespace StaticQL.Test
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
        let parsedBack = CommandEffect.OfSQL(userModel.Model, "anonymous", str)
        let fragmentsBack = userModel.Backend.ToCommandFragments(indexer, parsedBack.Statements)
        Assert.AreEqual(str, CommandFragment.Stringize(fragmentsBack))

    [<TestMethod>]
    member __.TestSelect() =
        roundtrip """
            select * from Users u where u.Id = 1
        """