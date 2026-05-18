// EF Core entity types that mirror the V1.model.sql schema from the Rezoom FileSystem demo.
namespace FileSystem.EFVersion.Entities;

public class User
{
    public int Id { get; set; }
    public string Email { get; set; } = null!;
    public string Name { get; set; } = null!;
}

public class Group
{
    public int Id { get; set; }
    public string Name { get; set; } = null!;
}

public class UserGroup
{
    public int UserId { get; set; }
    public int GroupId { get; set; }
}

public class RecycleItem
{
    public int Id { get; set; }
    public DateTime RecycledUtc { get; set; }
    public int RecycledById { get; set; }
}

public class Folder
{
    public int Id { get; set; }
    public int? ParentId { get; set; }
    public string Name { get; set; } = null!;
    public int? RecycleItemId { get; set; }
}

public class File
{
    public int Id { get; set; }
    public int ParentId { get; set; }
    public string Name { get; set; } = null!;
    public byte[] Content { get; set; } = null!;
    public int? RecycleItemId { get; set; }
}

public class FolderUserPermission
{
    public int FolderId { get; set; }
    public int UserId { get; set; }
    public bool? DeletePermission { get; set; }
    public bool? CreatePermission { get; set; }
}

public class FolderGroupPermission
{
    public int FolderId { get; set; }
    public int GroupId { get; set; }
    public bool? DeletePermission { get; set; }
    public bool? CreatePermission { get; set; }
}
