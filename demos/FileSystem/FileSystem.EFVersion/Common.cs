// Common types shared by every layer. Mirrors the F# FileSystem demo's Common.fs.
using System.Security;
using System.Text;

namespace FileSystem.EFVersion;

// Strongly typed wrappers for integer IDs.
// A must-have when we're doing all our domain logic by passing around IDs.
public readonly record struct UserId(int Value);
public readonly record struct FileId(int Value);
public readonly record struct FolderId(int Value);
public readonly record struct GroupId(int Value);
public readonly record struct RecycleItemId(int Value);

/// <summary>
/// Represents a permission that can be assigned at a level of the hierarchy.
/// This level of the hierarchy doesn't have to specify the permission, it can
/// just punt to another layer (via Inherit).
/// </summary>
public enum LocalPermission
{
    Allow,
    Deny,
    Inherit,
}

/// <summary>
/// Identifies a user-like entity permissions can be controlled for.
/// Currently this is either a user or group.
/// </summary>
public abstract record PermissionSubjectId
{
    private PermissionSubjectId() { }
    public sealed record ForUser(UserId UserId) : PermissionSubjectId;
    public sealed record ForGroup(GroupId GroupId) : PermissionSubjectId;
}

/// <summary>
/// Represents the permissions local to a folder and user or group ID.
/// In other words, a single record from the FolderPermissions table.
/// </summary>
public sealed record LocalPermissions(LocalPermission DeletePermission, LocalPermission CreatePermission)
{
    public static LocalPermissions Empty { get; } =
        new(LocalPermission.Inherit, LocalPermission.Inherit);
}

/// <summary>
/// Represents a permission that has been resolved taking into account inheritance.
/// After all, the bottom line is you either can or can't do it.
/// </summary>
public enum EffectivePermission
{
    Allowed,
    Denied,
}

public sealed record EffectivePermissions(
    FolderId? FolderId,
    UserId UserId,
    EffectivePermission Delete,
    EffectivePermission Create)
{
    public void AssertCanDelete()
    {
        if (Delete != EffectivePermission.Allowed)
            throw new SecurityException("Not allowed to delete");
    }

    public void AssertCanCreate()
    {
        if (Create != EffectivePermission.Allowed)
            throw new SecurityException("Not allowed to create");
    }

    public override string ToString()
    {
        var parts = new List<string>();
        if (Delete == EffectivePermission.Allowed) parts.Add("D");
        if (Create == EffectivePermission.Allowed) parts.Add("C");
        var joined = string.Join(",", parts);
        return joined.Length == 0 ? "DENIED" : joined;
    }
}

public sealed record FileData(FileId FileId, FolderId ParentId, string Name);

public sealed record FolderData(FolderId FolderId, FolderId? ParentId, string Name);

public abstract record FileOrFolder
{
    private FileOrFolder() { }

    public sealed record FileEntry(FileData Data) : FileOrFolder
    {
        public override string ToString() => Data.Name;
    }

    public sealed record FolderEntry(FolderData Data) : FileOrFolder
    {
        public override string ToString() => $"{Data.Name}/    ({Data.FolderId.Value})";
    }
}

public sealed record Hierarchy<TInfo>(
    FileOrFolder Node,
    IReadOnlyList<Hierarchy<TInfo>> Children,
    TInfo Info)
{
    private string Render(int depth)
    {
        var prefix = Node switch
        {
            FileOrFolder.FolderEntry _ => "|-",
            FileOrFolder.FileEntry _ => " *",
            _ => "  ",
        };
        var indent = new string(' ', depth) + prefix;
        var info = Info is null ? "" : " | " + Info.ToString();
        var sb = new StringBuilder();
        sb.Append(indent).Append(' ').Append(Node).Append(info);
        foreach (var child in Children)
        {
            sb.Append(Environment.NewLine);
            sb.Append(child.Render(depth + 2));
        }
        return sb.ToString();
    }

    public override string ToString() => Render(0);
}
