using NUnit.Framework;
using Rezoom.SQL.Compiler;
namespace Rezoom.SQL.Provider.Test;

/// <summary>
/// Confirms errors surface with SQ0XX codes intact
/// through the TP-as-library path.
/// </summary>
[TestFixture]
public class ErrorTests
{
    private ProviderHarness _harness = null!;

    [OneTimeSetUp]
    public void Setup() => _harness = new ProviderHarness("WithUserTypes");

    [OneTimeTearDown]
    public void TearDown() => _harness.Dispose();

    [Test]
    public void unknown_interface_in_select_clause_throws_SQ067()
    {
        var ex = Assert.Throws<SourceException>(() =>
            _harness.Generate("select<INotAnInterface> Name from Users"));
        Assert.That(ex!.Message, Does.Contain("SQ067"));
        Assert.That(ex.Message, Does.Contain("INotAnInterface"));
    }

    [Test]
    public void rowtypes_on_subquery_throws_SQ071()
    {
        var ex = Assert.Throws<SourceException>(() => _harness.Generate(
            "select * from (select<IUserSummary> Name, Email from Users) sq"));
        Assert.That(ex!.Message, Does.Contain("SQ071"));
    }

    [Test]
    public void rowtypes_on_non_leftmost_compound_throws_SQ070()
    {
        var ex = Assert.Throws<SourceException>(() => _harness.Generate(@"
            select 1 a, 2 b
            union all
            select<IUserSummary> 3, 4"));
        Assert.That(ex!.Message, Does.Contain("SQ070"));
    }
}
