using System.Diagnostics;
using System.Security;
using FileSystem.EFVersion;
using FileSystem.EFVersion.Entities;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;

// Configuration: appsettings.json + environment variables (which override).
// The connection string for "rzsql" comes from ConnectionStrings:rzsql.
var configuration = new ConfigurationBuilder()
    .AddJsonFile("appsettings.json", optional: false)
    .AddEnvironmentVariables()
    .Build();

var connectionString = configuration.GetConnectionString("rzsql")
    ?? throw new InvalidOperationException("Missing connection string 'rzsql'.");

var queryCounter = new QueryCounterInterceptor();

var services = new ServiceCollection();
services.AddSingleton<IConfiguration>(configuration);
services.AddDbContextFactory<FileSystemDbContext>(opts =>
    opts.UseSqlServer(connectionString).AddInterceptors(queryCounter));

await using var provider = services.BuildServiceProvider();
var factory = provider.GetRequiredService<IDbContextFactory<FileSystemDbContext>>();

// Create the schema if it doesn't exist (analogous to DemoSetup.migrate in the Rezoom version).
await using (var setupDb = await factory.CreateDbContextAsync())
{
    await DemoSetup.Migrate(setupDb);
}

await new Repl(factory, queryCounter).Run();

internal sealed class Repl
{
    private readonly IDbContextFactory<FileSystemDbContext> _factory;
    private readonly QueryCounterInterceptor _queryCounter;
    private UserId _userId;

    public Repl(IDbContextFactory<FileSystemDbContext> factory, QueryCounterInterceptor queryCounter)
    {
        _factory = factory;
        _queryCounter = queryCounter;
    }

    public async Task Run()
    {
        await Bench("Set up demo data", async db => await DemoSetup.SetUpDemoData(db));

        _userId = await BecomeDefaultUser();

        var looping = true;
        while (looping)
        {
            Console.Write("> ");
            var input = Console.ReadLine();
            if (input is null || input == "quit")
            {
                looping = false;
                continue;
            }
            var parts = input.Split(' ', StringSplitOptions.RemoveEmptyEntries);
            if (parts.Length == 0)
                continue;
            var cmd = parts[0];
            var args = parts.Skip(1).ToArray();
            try
            {
                await Bench(input, db => RunCommand(db, cmd, args));
            }
            catch (AggregateException agg) when (agg.InnerExceptions.Count == 1
                && agg.InnerException is SecurityException s1)
            {
                Console.Error.WriteLine($"Security exception: {s1.Message}");
            }
            catch (SecurityException s2)
            {
                Console.Error.WriteLine($"Security exception: {s2.Message}");
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine($"Exception: {ex}");
            }
        }
    }

    private async Task<UserId> BecomeDefaultUser()
    {
        UserId result = default;
        await Bench(
            $"Becoming default user {DemoSetup.DefaultUserName}",
            async db =>
            {
                var domain = NewDomain(db);
                var id = await domain.GetUserByName(DemoSetup.DefaultUserName);
                result = id ?? throw new InvalidOperationException(
                    $"Default user {DemoSetup.DefaultUserName} not found.");
            });
        return result;
    }

    private static Domain NewDomain(FileSystemDbContext db) => new(new Persistence(db));

    private async Task Bench(string description, Func<FileSystemDbContext, Task> op)
    {
        _queryCounter.Reset();
        var sw = Stopwatch.StartNew();
        await using var db = await _factory.CreateDbContextAsync();
        await op(db);
        sw.Stop();
        Console.WriteLine(
            $"  -> Ran `{description}` in {sw.ElapsedMilliseconds} ms with {_queryCounter.Count} round trips.");
    }

    private async Task RunCommand(FileSystemDbContext db, string cmd, string[] args)
    {
        var domain = NewDomain(db);
        switch (cmd, args.Length)
        {
            case ("reset", 0):
                await DemoSetup.SetUpDemoData(db);
                var defaultId = await domain.GetUserByName(DemoSetup.DefaultUserName);
                _userId = defaultId ?? throw new InvalidOperationException(
                    $"Default user {DemoSetup.DefaultUserName} not found.");
                return;
            case ("become", 1):
                var resolved = await domain.GetUserByName(args[0]);
                if (resolved is null)
                    Console.Error.WriteLine($"No such user {args[0]}");
                else
                    _userId = resolved.Value;
                return;
            case ("ls", 0):
                await ShowHierarchy(domain, null);
                return;
            case ("ls", 1):
                if (TryParseFolderId(args[0], out var fls))
                    await ShowHierarchy(domain, fls);
                return;
            case ("lsp", 0):
                await ShowHierarchyWithPermissions(domain, _userId, null);
                return;
            case ("lsp", 1):
                if (TryParseFolderId(args[0], out var flsp))
                    await ShowHierarchyWithPermissions(domain, _userId, flsp);
                return;
            case ("rmdir", 1):
                if (TryParseFolderId(args[0], out var frm))
                    await domain.RecycleFolder(_userId, frm);
                return;
            case ("testcache", 1):
                await TestCache(domain, _userId, args[0] == "doubleup");
                return;
            default:
                Console.Error.WriteLine("Unrecognized command.");
                return;
        }
    }

    private static bool TryParseFolderId(string text, out FolderId folderId)
    {
        if (int.TryParse(text, out var id))
        {
            folderId = new FolderId(id);
            return true;
        }
        Console.Error.WriteLine($"Misformatted id {text}");
        folderId = default;
        return false;
    }

    private static async Task ShowHierarchy(Domain domain, FolderId? fromParentId)
    {
        var hierarchies = await domain.GetHierarchy(fromParentId);
        foreach (var h in hierarchies)
            Console.WriteLine(h);
    }

    private static async Task<Hierarchy<EffectivePermissions>> AddPermissionsToHierarchy(
        Domain domain, UserId userId, Hierarchy<object?> hierarchy)
    {
        var ownPermissions = hierarchy.Node switch
        {
            FileOrFolder.FileEntry file => await domain.GetEffectivePermissions(userId, file.Data.ParentId),
            FileOrFolder.FolderEntry folder => await domain.GetEffectivePermissions(userId, folder.Data.FolderId),
            _ => throw new InvalidOperationException(),
        };
        var children = new List<Hierarchy<EffectivePermissions>>(hierarchy.Children.Count);
        foreach (var child in hierarchy.Children)
            children.Add(await AddPermissionsToHierarchy(domain, userId, child));
        return new Hierarchy<EffectivePermissions>(hierarchy.Node, children, ownPermissions);
    }

    private static async Task ShowHierarchyWithPermissions(Domain domain, UserId userId, FolderId? fromParentId)
    {
        var hierarchies = await domain.GetHierarchy(fromParentId);
        var withPermissions = new List<Hierarchy<EffectivePermissions>>(hierarchies.Count);
        foreach (var h in hierarchies)
            withPermissions.Add(await AddPermissionsToHierarchy(domain, userId, h));
        foreach (var h in withPermissions)
            Console.WriteLine(h);
    }

    private static async Task TestCache(Domain domain, UserId userId, bool doubleUp)
    {
        var folders = await domain.GetHierarchy(null);
        foreach (var h in folders)
            Console.WriteLine(h);
        if (doubleUp)
        {
            var foldersAgain = await domain.GetHierarchy(null);
            foreach (var h in foldersAgain)
                Console.WriteLine(h);
        }
        foreach (var root in folders)
        {
            foreach (var child in root.Children)
            {
                if (child.Node is FileOrFolder.FileEntry file)
                    await domain.RecycleFile(userId, file.Data.FileId);
            }
        }
        if (!doubleUp)
        {
            var foldersAfter = await domain.GetHierarchy(null);
            foreach (var h in foldersAfter)
                Console.WriteLine(h);
        }
    }
}
