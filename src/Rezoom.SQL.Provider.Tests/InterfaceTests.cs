using System.Linq;
using NUnit.Framework;
using TypeProviderUser.UserTypes;
namespace Rezoom.SQL.Provider.Test;

/// <summary>
/// SELECT rowtypes scenarios that have triggered bugs in the past.
/// </summary>
[TestFixture]
public class InterfaceTests
{
    private ProviderHarness _harness = null!;

    [OneTimeSetUp]
    public void Setup() => _harness = new ProviderHarness("WithUserTypes");

    [OneTimeTearDown]
    public void TearDown() => _harness.Dispose();

    [Test]
    public void flat_select_with_interface_implements_it()
    {
        var asm = _harness.LoadGenerated(
            "select<IUserSummary> Name, Email from Users");
        var rowTy = asm.GetTypes().Single(t => t.Name == "Row");
        Assert.That(
            rowTy.GetInterfaces().Select(i => i.FullName),
            Does.Contain(typeof(IUserSummary).FullName));
    }

    [Test]
    public void scalar_row_implements_both_user_interface_and_IScalar()
    {
        var asm = _harness.LoadGenerated(
            "select<IHasUserId> Id from Users");
        var rowTy = asm.GetTypes().Single(t => t.Name == "Row");
        var ifaceNames = rowTy.GetInterfaces()
            .Select(i => i.FullName ?? "")
            .ToList();
        Assert.That(ifaceNames, Does.Contain(typeof(IHasUserId).FullName));
        // IScalar<UserId> coexistence — auto-added for single-column rows.
        Assert.That(ifaceNames,
            Has.Some.StartsWith("Rezoom.SQL.IScalar"));
    }

    [Test]
    public void many_nav_recursion_emits_without_DeclaringType_error()
    {
        // "No DeclaringType" error from convMethodRefToTgt surfaced
        // here; tests for the explicit-impl AddMember.
        Assert.DoesNotThrow(() => _harness.Generate(@"
            select<IUserWithArticles>
                u.*,
                many Articles(a.Id, a.ArticleTitle, a.ArticleText)
            from Users u
            left join Articles a on a.AuthorId = u.Id"));
    }

    [Test]
    public void many_nav_with_IEnumerable_interface_handles_cross_variance()
    {
        Assert.DoesNotThrow(() => _harness.Generate(@"
            select<IUserWithArticlesEnumerable>
                u.*,
                many Articles(a.Id, a.ArticleTitle, a.ArticleText)
            from Users u
            left join Articles a on a.AuthorId = u.Id"));
    }

    [Test]
    public void one_nav_with_nested_interface_emits_cleanly()
    {
        Assert.DoesNotThrow(() => _harness.Generate(@"
            select<IArticleWithAuthor>
                a.*,
                one Author(u.*)
            from Articles a
            join Users u on u.Id = a.AuthorId"));
    }

    [Test]
    public void optional_nav_emits_without_MakeGenericMethod_error()
    {
        // The MLC + standard-reflection MakeGenericMethod mismatch fired
        // here; tests for the ProvidedTypeBuilder.MakeGenericMethod fix.
        Assert.DoesNotThrow(() => _harness.Generate(@"
            select<IUserWithMaybePicture>
                u.*,
                optional Picture(p.*)
            from Users u
            left join Pictures p on p.SHA256 = u.ProfilePictureSHA256"));
    }
}
