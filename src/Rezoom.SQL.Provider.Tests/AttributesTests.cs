using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using Microsoft.FSharp.Core;
using NUnit.Framework;
using Rezoom.SQL.Compiler;
using Rezoom.SQL.Mapping;

namespace Rezoom.SQL.Provider.Test;

/// <summary>
/// Verifies that the Rezoom.SQL.Annotations attributes flow from a
/// user-types library through the loader into UserPrimitiveType's
/// RawBackendSQLType / SQLTypeLength fields.
///
/// These tests drive UserModel.Load directly rather than going through
/// the TP.
/// </summary>
[TestFixture]
public class AttributesTests
{
    private UserTypeLibrary _lib = null!;
    private ProviderHarness _harness = null!;

    [OneTimeSetUp]
    public void Setup()
    {
        var fixtureDir = Path.Combine(
            AppContext.BaseDirectory, "Fixtures", "WithUserTypes");
        var refs = AppDomain.CurrentDomain.GetAssemblies()
            .Where(a => !a.IsDynamic && !string.IsNullOrEmpty(a.Location))
            .Select(a => a.Location)
            .Concat(Directory.EnumerateFiles(
                AppContext.BaseDirectory, "*.dll", SearchOption.TopDirectoryOnly))
            .Distinct(StringComparer.OrdinalIgnoreCase);
        var userModel = UserModel.Load(fixtureDir, "", refs);
        _lib = userModel.UserTypeLibrary;
        // Used by the end-to-end fragment-emission tests at the bottom
        // of this class.
        _harness = new ProviderHarness("WithUserTypes");
    }

    [OneTimeTearDown]
    public void TearDown() => _harness.Dispose();

    /// <summary>
    /// Look up a primitive by its UserCLRType FullName. AllPrimitives is the
    /// only fully-public accessor that doesn't require constructing the
    /// runtime Type (the MLC-loaded Type is a different object).
    /// </summary>
    private UserPrimitiveType FindPrimitive(string fullName)
        => _lib.AllPrimitives.Single(p => p.UserCLRType.FullName == fullName);

    [Test]
    public void single_case_du_with_RawBackendSQLType_is_picked_up()
    {
        var p = FindPrimitive("TypeProviderUser.UserTypes.CompactInt");
        Assert.That(p.RawBackendSQLType,
            Is.EqualTo(FSharpOption<string>.Some("MEDIUMINT")));
        Assert.That(p.SQLTypeLength, Is.EqualTo(FSharpOption<int>.None));
    }

    [Test]
    public void single_case_du_with_SQLTypeLength_is_picked_up()
    {
        var p = FindPrimitive("TypeProviderUser.UserTypes.ShortName");
        Assert.That(p.SQLTypeLength,
            Is.EqualTo(FSharpOption<int>.Some(80)));
        Assert.That(p.RawBackendSQLType, Is.EqualTo(FSharpOption<string>.None));
    }

    [Test]
    public void method_level_attribute_on_extension_ToPrimitive_is_picked_up()
    {
        // The attribute is on ToPrimitive in DateTimeOffsetExtensions,
        // since System.DateTimeOffset can't be decorated directly.
        // Loader's explicit-path resolveExplicit should still find it.
        var p = FindPrimitive("System.DateTimeOffset");
        Assert.That(p.RawBackendSQLType,
            Is.EqualTo(FSharpOption<string>.Some("DATETIMEOFFSET(7)")));
    }

    // The bad-fixture class lives here in the test assembly rather than
    // in the shared user-types fixture, so its presence doesn't trip the
    // validation during normal scenario load.

    [Rezoom.SQL.Annotations.RawBackendSQLType("VARCHAR(80)")]
    [Rezoom.SQL.Annotations.SQLTypeLength(80)]
    private sealed class DoubleAnnotatedTarget { }

    // Verify attributed types show up in the output fragments.

    private static string StringizeFragmentsOf(Assembly asm, string typeName)
    {
        var commandTy = asm.GetTypes().Single(t => t.Name == typeName);
        var commandFactory = commandTy.GetMethod(
            "Command", BindingFlags.Public | BindingFlags.Static)!;
        var cmd = commandFactory.Invoke(null, Array.Empty<object>())!;
        var fragments = cmd.GetType().GetProperty("Fragments")!.GetValue(cmd);
        // CommandFragment.Stringize takes IEnumerable<CommandFragment>;
        // the runtime type is IReadOnlyList<CommandFragment>, which is
        // assignable.
        return CommandFragment.Stringize(
            (IEnumerable<CommandFragment>)fragments!);
    }

    [Test]
    public void RawBackendSQLType_reaches_emitted_CAST_in_generated_command()
    {
        var asm = _harness.LoadGenerated(
            "select cast(42 as CompactInt) as c", "CastToCompactInt");
        var sql = StringizeFragmentsOf(asm, "CastToCompactInt");
        Assert.That(sql, Does.Contain("MEDIUMINT"));
        // Belt-and-suspenders: the literal made it through verbatim,
        // not e.g. the default INT mapping for the underlying int.
        Assert.That(sql, Does.Not.Contain("INT NOT NULL"));
    }

    [Test]
    public void both_attributes_on_same_primitive_throws_at_resolve_time()
    {
        var ex = Assert.Throws<Exception>(() =>
            UserTypeAnnotations.resolveType(typeof(DoubleAnnotatedTarget)));
        Assert.That(ex!.Message, Does.Contain("mutually exclusive"));
        Assert.That(ex.Message, Does.Contain("RawBackendSQLType"));
        Assert.That(ex.Message, Does.Contain("SQLTypeLength"));
    }
}
