// Mirrors DemoSetup.fs from the Rezoom FileSystem demo: ensures the schema exists,
// nukes any previous demo data, then re-seeds folders, files, users, groups, and permissions.
using System.Text;
using FileSystem.EFVersion.Entities;
using Microsoft.EntityFrameworkCore;

namespace FileSystem.EFVersion;

public static class DemoSetup
{
    private sealed record FolderStructureEntry(string Name, bool IsFolder, List<FolderStructureEntry> Children);

    private static FolderStructureEntry FileEntry(string name) => new(name, IsFolder: false, Children: new());
    private static FolderStructureEntry DirEntry(string name, List<FolderStructureEntry> children) =>
        new(name, IsFolder: true, Children: children);

    private static readonly List<FolderStructureEntry> DemoFolderStructure = new()
    {
        DirEntry("root", new()
        {
            DirEntry("a", new()
            {
                DirEntry("a.1", new()
                {
                    FileEntry("a.1.1"),
                    FileEntry("a.1.2"),
                    FileEntry("a.1.3"),
                }),
                FileEntry("a.2"),
                FileEntry("a.3"),
                DirEntry("a.4", new()
                {
                    FileEntry("a.4.1"),
                    FileEntry("a.4.2"),
                    FileEntry("a.4.3"),
                    FileEntry("a.4.5"),
                }),
            }),
            DirEntry("b", new()
            {
                DirEntry("b.1", new()
                {
                    FileEntry("b.1.1"),
                    FileEntry("b.1.2"),
                    DirEntry("b.1.3", new()
                    {
                        FileEntry("b.1.3.1"),
                        FileEntry("b.1.3.2"),
                        DirEntry("b.1.3.3", new()
                        {
                            FileEntry("b.1.3.3.1"),
                            FileEntry("b.1.3.3.2"),
                        }),
                    }),
                }),
                DirEntry("b.2", new()
                {
                    DirEntry("b.2.1", new()
                    {
                        FileEntry("b.2.1.1"),
                    }),
                    FileEntry("b.2.2"),
                    FileEntry("b.2.3"),
                }),
                FileEntry("b.3"),
                FileEntry("b.4"),
                FileEntry("b.5"),
                FileEntry("b.6"),
            }),
            FileEntry("c"),
            FileEntry("d"),
        }),
    };

    private abstract record DemoPermission
    {
        private DemoPermission() { }
        public sealed record AllowUnder(string FolderName) : DemoPermission;
        public sealed record DenyUnder(string FolderName) : DemoPermission;
    }

    private static readonly List<(string Name, List<DemoPermission> Permissions)> DemoUsers = new()
    {
        ("Kelly", new() { new DemoPermission.AllowUnder("root") }),
        ("Graham", new() { new DemoPermission.AllowUnder("a"), new DemoPermission.DenyUnder("a.3") }),
        ("Robert", new() { new DemoPermission.AllowUnder("b.1") }),
        ("John", new()
        {
            new DemoPermission.AllowUnder("b"),
            new DemoPermission.DenyUnder("b.1"),
            new DemoPermission.AllowUnder("b.1.3"),
        }),
        ("Sam", new()),
        ("Christopher", new() { new DemoPermission.AllowUnder("b.2.1") }),
    };

    private static readonly List<(string Name, List<DemoPermission> Permissions, List<string> Members)> DemoGroups = new()
    {
        ("NotB2A4",
            new()
            {
                new DemoPermission.AllowUnder("root"),
                new DemoPermission.DenyUnder("b.2"),
                new DemoPermission.DenyUnder("a.4"),
            },
            new() { "Sam", "Christopher" }),
    };

    public static string DefaultUserName => DemoUsers[0].Name;

    public static Task Migrate(FileSystemDbContext db) => db.Database.EnsureCreatedAsync();

    public static async Task SetUpDemoData(FileSystemDbContext db)
    {
        await NukeData(db);
        await SetUpFolders(db, parentId: null, DemoFolderStructure);
        foreach (var (name, perms) in DemoUsers)
            await SetUpDemoUser(db, name, perms);
        foreach (var (name, perms, members) in DemoGroups)
            await SetUpDemoGroup(db, name, perms, members);
    }

    private static async Task NukeData(FileSystemDbContext db)
    {
        await db.Database.ExecuteSqlRawAsync("delete from Files");
        await db.Database.ExecuteSqlRawAsync("delete from FolderUserPermissions");
        await db.Database.ExecuteSqlRawAsync("delete from FolderGroupPermissions");
        // Folders has a self-referencing FK; null parents out first so the delete succeeds.
        await db.Database.ExecuteSqlRawAsync("update Folders set ParentId = null where ParentId is not null");
        await db.Database.ExecuteSqlRawAsync("delete from Folders");
        await db.Database.ExecuteSqlRawAsync("delete from RecycleItems");
        await db.Database.ExecuteSqlRawAsync("delete from UserGroups");
        await db.Database.ExecuteSqlRawAsync("delete from Groups");
        await db.Database.ExecuteSqlRawAsync("delete from Users");
    }

    private static async Task SetUpFolders(FileSystemDbContext db, int? parentId, List<FolderStructureEntry> entries)
    {
        foreach (var entry in entries)
        {
            if (entry.IsFolder)
            {
                var folder = new Folder { ParentId = parentId, Name = entry.Name };
                db.Folders.Add(folder);
                await db.SaveChangesAsync();
                await SetUpFolders(db, folder.Id, entry.Children);
            }
            else
            {
                var content = Encoding.UTF8.GetBytes("This is the content of " + entry.Name);
                db.Files.Add(new Entities.File
                {
                    ParentId = parentId!.Value,
                    Name = entry.Name,
                    Content = content,
                });
                await db.SaveChangesAsync();
            }
        }
    }

    private static async Task SetUpDemoUser(FileSystemDbContext db, string name, List<DemoPermission> permissions)
    {
        var user = new User { Name = name, Email = name + "@example.com" };
        db.Users.Add(user);
        await db.SaveChangesAsync();
        foreach (var permission in permissions)
        {
            var (allow, folderName) = permission switch
            {
                DemoPermission.AllowUnder a => (true, a.FolderName),
                DemoPermission.DenyUnder d => (false, d.FolderName),
                _ => throw new InvalidOperationException(),
            };
            // Match the F# INSERT ... SELECT semantics: silently skip when no folder matches.
            var folderId = await db.Folders
                .Where(f => f.Name == folderName)
                .Select(f => (int?)f.Id)
                .FirstOrDefaultAsync();
            if (folderId is null) continue;
            db.FolderUserPermissions.Add(new FolderUserPermission
            {
                FolderId = folderId.Value,
                UserId = user.Id,
                DeletePermission = allow,
                CreatePermission = allow,
            });
            await db.SaveChangesAsync();
        }
    }

    private static async Task SetUpDemoGroup(
        FileSystemDbContext db,
        string name,
        List<DemoPermission> permissions,
        List<string> members)
    {
        var group = new Group { Name = name };
        db.Groups.Add(group);
        await db.SaveChangesAsync();

        foreach (var permission in permissions)
        {
            var (allow, folderName) = permission switch
            {
                DemoPermission.AllowUnder a => (true, a.FolderName),
                DemoPermission.DenyUnder d => (false, d.FolderName),
                _ => throw new InvalidOperationException(),
            };
            // Match the F# INSERT ... SELECT semantics: silently skip when no folder matches.
            var folderId = await db.Folders
                .Where(f => f.Name == folderName)
                .Select(f => (int?)f.Id)
                .FirstOrDefaultAsync();
            if (folderId is null) continue;
            db.FolderGroupPermissions.Add(new FolderGroupPermission
            {
                FolderId = folderId.Value,
                GroupId = group.Id,
                DeletePermission = allow,
                CreatePermission = allow,
            });
            await db.SaveChangesAsync();
        }

        foreach (var memberName in members)
        {
            // Match the F# INSERT ... SELECT semantics: silently skip when no user matches.
            var userId = await db.Users
                .Where(u => u.Name == memberName)
                .Select(u => (int?)u.Id)
                .FirstOrDefaultAsync();
            if (userId is null) continue;
            db.UserGroups.Add(new UserGroup { UserId = userId.Value, GroupId = group.Id });
            await db.SaveChangesAsync();
        }
    }
}
