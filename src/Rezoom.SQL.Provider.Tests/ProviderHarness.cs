using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading;
using Microsoft.FSharp.Core;
using Microsoft.FSharp.Core.CompilerServices;
using Rezoom.SQL.Provider;
namespace Rezoom.SQL.Provider.Test;

/// <summary>
/// One scenario = one fixture directory + one Provider instance.
/// Call <see cref="Generate"/> to apply static args + force IL emission, or
/// <see cref="LoadGenerated"/> to also get a reflectable Assembly back.
/// </summary>
public sealed class ProviderHarness : IDisposable
{
    private readonly Provider _provider;
    private readonly ITypeProvider _itp;
    private readonly Type _sqlParametric;
    private int _counter;

    public ProviderHarness(string fixtureName)
    {
        var fixtureDir = Path.Combine(
            AppContext.BaseDirectory, "Fixtures", fixtureName);
        if (!Directory.Exists(fixtureDir))
            throw new DirectoryNotFoundException(
                $"Fixture not found: {fixtureDir}. " +
                "Did the csproj <Content Include=\"Fixtures\\**\"> copy run?");

        var cfg = new TypeProviderConfig(
            systemRuntimeContainsType:
                FuncConvert.FromFunc<string, bool>(_ => true))
        {
            ResolutionFolder = fixtureDir,
            RuntimeAssembly = typeof(Provider).Assembly.Location,
            ReferencedAssemblies = GatherReferencedAssemblies(),
            TemporaryFolder = Path.GetTempPath(),
            IsHostedExecution = false,
            IsInvalidationSupported = false,
        };

        _provider = new Provider(cfg);
        _itp = _provider;

        var ns = _itp.GetNamespaces()
            .Single(n => n.NamespaceName == "Rezoom.SQL");
        _sqlParametric = ns.GetTypes().Single(t => t.Name == "SQL");
    }

    /// <summary>
    /// Apply static args + force IL emission. Returns raw bytes.
    /// Throws whatever the TP throws if codegen fails.
    /// </summary>
    public byte[] Generate(string sql, string? typeName = null)
    {
        typeName ??= $"Generated_{Interlocked.Increment(ref _counter)}";
        // SQL<...> has TWO static parameters: (sql, model). The model
        // string is empty when implicit-discovery of rzsql.json under
        // ResolutionFolder is intended (which is what every test wants).
        var concrete = _itp.ApplyStaticArguments(
            _sqlParametric,
            new[] { "Rezoom.SQL", typeName },
            new object[] { sql, "" });
        // Where rubber meets the road, "did the TP actually emit a loadable assembly"?
        // AssemblyCompiler.Compile walks every member and lowers it to IL.
        // Most codegen bugs will throw here.
        return _itp.GetGeneratedAssemblyContents(concrete.Assembly);
    }

    /// <summary>Generate + Assembly.Load so the test can reflect on output.</summary>
    public Assembly LoadGenerated(string sql, string? typeName = null)
        => Assembly.Load(Generate(sql, typeName));

    /// <summary>
    /// Harvest both the loaded assembly set AND every DLL the build copied
    /// to the test output directory. Loaded-only misses transitive
    /// references that haven't been touched yet at test-start time
    /// (Rezoom.SQL.Mapping in particular only loads on first use, so the
    /// TP's target-resolution can't find the Command type).
    /// </summary>
    private static string[] GatherReferencedAssemblies()
    {
        var loaded = AppDomain.CurrentDomain.GetAssemblies()
            .Where(a => !a.IsDynamic && !string.IsNullOrEmpty(a.Location))
            .Select(a => a.Location);
        var copiedToOutput = Directory.EnumerateFiles(
            AppContext.BaseDirectory, "*.dll", SearchOption.TopDirectoryOnly);
        return loaded.Concat(copiedToOutput)
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .ToArray();
    }

    public void Dispose()
    {
        // Provider inherits TypeProviderForNamespaces, which is IDisposable;
        // it owns the per-instance UserModelCache and indirectly the
        // module-level MLC cache for the lifetime of the AppDomain.
        (_provider as IDisposable)?.Dispose();
    }
}
