using System;
using System.IO;
using System.Linq;
using Microsoft.FSharp.Collections;
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
/// the TP — the attribute pipeline lives in Rezoom.SQL.Mapping, so the
/// loader is the correct seam to assert against.
/// </summary>
[TestFixture]
public class AttributesTests
{
    private UserTypeLibrary _lib = null!;

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
    }

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

    // --- Negative: mutual exclusion -------------------------------------
    //
    // The bad-fixture class lives here in the test assembly rather than
    // in the shared user-types fixture, so its presence doesn't trip the
    // validation during normal scenario load.

    [Rezoom.SQL.Annotations.RawBackendSQLType("VARCHAR(80)")]
    [Rezoom.SQL.Annotations.SQLTypeLength(80)]
    private sealed class DoubleAnnotatedTarget { }

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
