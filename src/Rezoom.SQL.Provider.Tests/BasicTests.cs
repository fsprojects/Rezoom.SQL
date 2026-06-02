using System.Linq;
using NUnit.Framework;
namespace Rezoom.SQL.Provider.Test;

/// <summary>
/// Minimal-userTypes scenarios.
/// </summary>
[TestFixture]
public class BasicTests
{
    private ProviderHarness _harness = null!;

    [OneTimeSetUp]
    public void Setup() => _harness = new ProviderHarness("Flat");

    [OneTimeTearDown]
    public void TearDown() => _harness.Dispose();

    [Test]
    public void simple_select_emits_row_with_columns()
    {
        var asm = _harness.LoadGenerated("select * from Users");
        var rowTy = asm.GetTypes().Single(t => t.Name == "Row");
        Assert.That(rowTy.GetProperties(), Is.Not.Empty);
        Assert.That(rowTy.GetProperty("Name"), Is.Not.Null);
        Assert.That(rowTy.GetProperty("Id"), Is.Not.Null);
        Assert.That(rowTy.GetProperty("Email"), Is.Not.Null);
    }

    [Test]
    public void emission_does_not_throw_for_a_handful_of_shapes()
    {
        Assert.DoesNotThrow(() =>
            _harness.Generate("select Id, Name from Users"));
        Assert.DoesNotThrow(() => _harness.Generate(@"
            select u.*, many Articles(a.Id, a.ArticleTitle)
            from Users u
            left join Articles a on a.AuthorId = u.Id"));
        Assert.DoesNotThrow(() => _harness.Generate(@"
            select a.*, one Author(u.*)
            from Articles a
            join Users u on u.Id = a.AuthorId"));
    }
}
