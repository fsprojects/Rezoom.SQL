// Domain logic mirroring the F# FileSystem demo's Domain.fs.
// Methods read/write through the Persistence layer and compose with no manual batching,
// no manual caching, and no manually-tuned "batch friendly" parameter threading.
namespace FileSystem.EFVersion;

public sealed class Domain
{
    private readonly Persistence _persistence;

    public Domain(Persistence persistence)
    {
        _persistence = persistence;
    }

    /// <summary>Direct passthrough to the persistence layer -- no extra logic needed here.</summary>
    public Task<UserId?> GetUserByName(string name) => _persistence.GetUserByName(name);

    public async Task<List<Hierarchy<object?>>> GetHierarchy(FolderId? rootId)
    {
        var children = await _persistence.GetChildren(rootId);
        var result = new List<Hierarchy<object?>>(children.Count);
        foreach (var child in children)
        {
            var grandchildren = child switch
            {
                FileOrFolder.FileEntry _ => new List<Hierarchy<object?>>(),
                FileOrFolder.FolderEntry folder => await GetHierarchy(folder.Data.FolderId),
                _ => throw new InvalidOperationException(),
            };
            result.Add(new Hierarchy<object?>(child, grandchildren, null));
        }
        return result;
    }

    /// <summary>
    /// The rule we use to combine local permissions across the groups a user is in.
    /// In short, if ANY group you are in explicitly denies a permission, it's denied.
    /// </summary>
    private static LocalPermission DenyWins(LocalPermission left, LocalPermission right) =>
        (left, right) switch
        {
            (LocalPermission.Allow, LocalPermission.Allow) => LocalPermission.Allow,
            (LocalPermission.Deny, _) => LocalPermission.Deny,
            (_, LocalPermission.Deny) => LocalPermission.Deny,
            (LocalPermission.Inherit, var x) => x,
            (var x, LocalPermission.Inherit) => x,
            _ => left,
        };

    /// <summary>
    /// The rule we use to combine local permissions between parent and child folders.
    /// In short, the child permission is used unless it is <see cref="LocalPermission.Inherit"/>.
    /// </summary>
    private static LocalPermission ChildWins(LocalPermission parent, LocalPermission child) =>
        child == LocalPermission.Inherit ? parent : child;

    private static LocalPermissions CombineBy(
        Func<LocalPermission, LocalPermission, LocalPermission> rule,
        LocalPermissions parent,
        LocalPermissions child) =>
        new(
            DeletePermission: rule(parent.DeletePermission, child.DeletePermission),
            CreatePermission: rule(parent.CreatePermission, child.CreatePermission));

    private static EffectivePermission Effective(EffectivePermission parent, LocalPermission local) =>
        local switch
        {
            LocalPermission.Inherit => parent,
            LocalPermission.Deny => EffectivePermission.Denied,
            LocalPermission.Allow => EffectivePermission.Allowed,
            _ => parent,
        };

    private async Task<LocalPermissions> GetCombinedGroupPermissions(FolderId folderId, List<GroupId> groupIds)
    {
        var combined = LocalPermissions.Empty;
        foreach (var groupId in groupIds)
        {
            var perms = await _persistence.GetLocalPermissions(folderId, new PermissionSubjectId.ForGroup(groupId));
            combined = CombineBy(DenyWins, combined, perms);
        }
        return combined;
    }

    /// <summary>Without any permission records in the database, everything is denied.</summary>
    private const EffectivePermission DefaultPermission = EffectivePermission.Denied;

    private static EffectivePermissions RootPermissions(UserId userId) =>
        new(FolderId: null, UserId: userId, Delete: DefaultPermission, Create: DefaultPermission);

    public async Task<EffectivePermissions> GetEffectivePermissions(UserId userId, FolderId folderId)
    {
        var folder = await _persistence.GetFolder(folderId);
        var groupIds = await _persistence.GetUserGroupIds(userId);

        var parentPermissions = folder.ParentId.HasValue
            ? await GetEffectivePermissions(userId, folder.ParentId.Value)
            : RootPermissions(userId);

        var userPermissions = await _persistence.GetLocalPermissions(folderId, new PermissionSubjectId.ForUser(userId));
        var groupPermissions = await GetCombinedGroupPermissions(folderId, groupIds);

        // if we have both permissions from our group and permissions specifically for us, the ones
        // applied to us take priority.
        var localPermissions = CombineBy(ChildWins, groupPermissions, userPermissions);

        // if we have both parent permissions and permissions specifically for this child folder,
        // the ones applied to the child folder take priority.
        return new EffectivePermissions(
            FolderId: folderId,
            UserId: userId,
            Create: Effective(parentPermissions.Create, localPermissions.CreatePermission),
            Delete: Effective(parentPermissions.Delete, localPermissions.DeletePermission));
    }

    public async Task RecycleFile(UserId userId, FileId fileId)
    {
        var self = await _persistence.GetFile(fileId);
        var permissions = await GetEffectivePermissions(userId, self.ParentId);
        permissions.AssertCanDelete();

        var recycleId = await _persistence.AddRecycleItem(userId);
        await _persistence.SetFileRecycleId(fileId, recycleId);
    }

    public async Task RecycleFolder(UserId userId, FolderId folderId)
    {
        var self = await _persistence.GetFolder(folderId);
        if (!self.ParentId.HasValue)
            throw new InvalidOperationException("Cannot recycle the root folder!");
        var permissions = await GetEffectivePermissions(userId, self.ParentId.Value);
        permissions.AssertCanDelete();

        var children = await _persistence.GetChildren(folderId);
        foreach (var child in children)
        {
            switch (child)
            {
                case FileOrFolder.FileEntry file:
                    await RecycleFile(userId, file.Data.FileId);
                    break;
                case FileOrFolder.FolderEntry folder:
                    await RecycleFolder(userId, folder.Data.FolderId);
                    break;
            }
        }

        var recycleId = await _persistence.AddRecycleItem(userId);
        await _persistence.SetFolderRecycleId(folderId, recycleId);
    }
}
