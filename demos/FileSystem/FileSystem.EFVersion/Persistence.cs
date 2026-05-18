// The persistence layer mirrors the F# FileSystem demo's Persistence.fs.
// In theory we could implement this set of methods against something other than a SQL database,
// including in memory for testing.
using FileSystem.EFVersion.Entities;
using Microsoft.EntityFrameworkCore;

namespace FileSystem.EFVersion;

public sealed class Persistence
{
    private readonly FileSystemDbContext _db;

    public Persistence(FileSystemDbContext db)
    {
        _db = db;
    }

    /// <summary>Get a user ID by their name.</summary>
    public async Task<UserId?> GetUserByName(string name)
    {
        var row = await _db.Users.AsNoTracking()
            .Where(u => u.Name == name)
            .Select(u => new { u.Id })
            .SingleOrDefaultAsync();
        return row is null ? null : new UserId(row.Id);
    }

    /// <summary>Get data about a single file.</summary>
    public async Task<FileData> GetFile(FileId id)
    {
        var row = await _db.Files.AsNoTracking()
            .Where(f => f.Id == id.Value)
            .Select(f => new { f.Id, f.Name, f.ParentId })
            .SingleAsync();
        return new FileData(new FileId(row.Id), new FolderId(row.ParentId), row.Name);
    }

    /// <summary>Get data about a single folder.</summary>
    public async Task<FolderData> GetFolder(FolderId id)
    {
        var row = await _db.Folders.AsNoTracking()
            .Where(f => f.Id == id.Value)
            .Select(f => new { f.Id, f.Name, f.ParentId })
            .SingleAsync();
        return new FolderData(
            new FolderId(row.Id),
            row.ParentId.HasValue ? new FolderId(row.ParentId.Value) : null,
            row.Name);
    }

    /// <summary>Get data for the non-recycled children of a folder.</summary>
    public async Task<List<FileOrFolder>> GetChildren(FolderId? parentId)
    {
        var pid = parentId?.Value;
        var folderRows = await _db.Folders.AsNoTracking()
            .Where(f => f.ParentId == pid && f.RecycleItemId == null)
            .Select(f => new { f.Id, f.Name })
            .ToListAsync();

        var fileRows = parentId.HasValue
            ? await _db.Files.AsNoTracking()
                .Where(f => f.ParentId == parentId.Value.Value && f.RecycleItemId == null)
                .Select(f => new { f.Id, f.Name })
                .ToListAsync()
            : new();

        var result = new List<FileOrFolder>(folderRows.Count + fileRows.Count);
        foreach (var f in folderRows)
            result.Add(new FileOrFolder.FolderEntry(new FolderData(new FolderId(f.Id), parentId, f.Name)));
        foreach (var f in fileRows)
            result.Add(new FileOrFolder.FileEntry(new FileData(new FileId(f.Id), parentId!.Value, f.Name)));
        return result;
    }

    /// <summary>Get the IDs of the groups a user is a member of.</summary>
    public async Task<List<GroupId>> GetUserGroupIds(UserId userId)
    {
        var ids = await _db.UserGroups.AsNoTracking()
            .Where(ug => ug.UserId == userId.Value)
            .Select(ug => ug.GroupId)
            .ToListAsync();
        return ids.Select(id => new GroupId(id)).ToList();
    }

    /// <summary>
    /// Get the data from the permissions record associated with a given folder and permission subject.
    /// If no permissions are specified for the particular combination, returns <see cref="LocalPermissions.Empty"/>.
    /// </summary>
    public async Task<LocalPermissions> GetLocalPermissions(FolderId folderId, PermissionSubjectId subject)
    {
        switch (subject)
        {
            case PermissionSubjectId.ForUser u:
            {
                var row = await _db.FolderUserPermissions.AsNoTracking()
                    .Where(p => p.FolderId == folderId.Value && p.UserId == u.UserId.Value)
                    .Select(p => new { p.DeletePermission, p.CreatePermission })
                    .SingleOrDefaultAsync();
                return row is null
                    ? LocalPermissions.Empty
                    : new LocalPermissions(
                        PermissionFromBool(row.DeletePermission),
                        PermissionFromBool(row.CreatePermission));
            }
            case PermissionSubjectId.ForGroup g:
            {
                var row = await _db.FolderGroupPermissions.AsNoTracking()
                    .Where(p => p.FolderId == folderId.Value && p.GroupId == g.GroupId.Value)
                    .Select(p => new { p.DeletePermission, p.CreatePermission })
                    .SingleOrDefaultAsync();
                return row is null
                    ? LocalPermissions.Empty
                    : new LocalPermissions(
                        PermissionFromBool(row.DeletePermission),
                        PermissionFromBool(row.CreatePermission));
            }
            default:
                throw new ArgumentOutOfRangeException(nameof(subject));
        }
    }

    /// <summary>Add a recycle item to the persistence store and return its ID.</summary>
    public async Task<RecycleItemId> AddRecycleItem(UserId recycler)
    {
        var item = new RecycleItem
        {
            RecycledUtc = DateTime.UtcNow,
            RecycledById = recycler.Value,
        };
        _db.RecycleItems.Add(item);
        await _db.SaveChangesAsync();
        return new RecycleItemId(item.Id);
    }

    /// <summary>Set the recycle item ID of a folder.</summary>
    public async Task SetFolderRecycleId(FolderId folderId, RecycleItemId? recycleId)
    {
        var folder = await _db.Folders.SingleAsync(f => f.Id == folderId.Value);
        folder.RecycleItemId = recycleId?.Value;
        await _db.SaveChangesAsync();
    }

    /// <summary>Set the recycle item ID of a file.</summary>
    public async Task SetFileRecycleId(FileId fileId, RecycleItemId? recycleId)
    {
        var file = await _db.Files.SingleAsync(f => f.Id == fileId.Value);
        file.RecycleItemId = recycleId?.Value;
        await _db.SaveChangesAsync();
    }

    private static LocalPermission PermissionFromBool(bool? b) => b switch
    {
        null => LocalPermission.Inherit,
        true => LocalPermission.Allow,
        false => LocalPermission.Deny,
    };
}
