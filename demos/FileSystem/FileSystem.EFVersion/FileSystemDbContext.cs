using Microsoft.EntityFrameworkCore;

namespace FileSystem.EFVersion.Entities;

public class FileSystemDbContext : DbContext
{
    public FileSystemDbContext(DbContextOptions<FileSystemDbContext> options) : base(options) { }

    public DbSet<User> Users => Set<User>();
    public DbSet<Group> Groups => Set<Group>();
    public DbSet<UserGroup> UserGroups => Set<UserGroup>();
    public DbSet<RecycleItem> RecycleItems => Set<RecycleItem>();
    public DbSet<Folder> Folders => Set<Folder>();
    public DbSet<File> Files => Set<File>();
    public DbSet<FolderUserPermission> FolderUserPermissions => Set<FolderUserPermission>();
    public DbSet<FolderGroupPermission> FolderGroupPermissions => Set<FolderGroupPermission>();

    protected override void OnModelCreating(ModelBuilder b)
    {
        b.Entity<User>(e =>
        {
            e.HasKey(x => x.Id);
            e.Property(x => x.Email).HasMaxLength(256).IsRequired();
            e.HasIndex(x => x.Email).IsUnique();
            e.Property(x => x.Name).HasMaxLength(64).IsRequired();
        });

        b.Entity<Group>(e =>
        {
            e.HasKey(x => x.Id);
            e.Property(x => x.Name).HasMaxLength(64).IsRequired();
            e.HasIndex(x => x.Name).IsUnique();
        });

        b.Entity<UserGroup>(e =>
        {
            e.HasKey(x => new { x.UserId, x.GroupId });
            e.HasOne<User>().WithMany().HasForeignKey(x => x.UserId);
            e.HasOne<Group>().WithMany().HasForeignKey(x => x.GroupId);
        });

        b.Entity<RecycleItem>(e =>
        {
            e.HasKey(x => x.Id);
            e.HasOne<User>().WithMany().HasForeignKey(x => x.RecycledById);
        });

        b.Entity<Folder>(e =>
        {
            e.HasKey(x => x.Id);
            e.Property(x => x.Name).HasMaxLength(128).IsRequired();
            e.HasOne<Folder>().WithMany().HasForeignKey(x => x.ParentId);
            e.HasOne<RecycleItem>().WithMany().HasForeignKey(x => x.RecycleItemId);
        });

        b.Entity<File>(e =>
        {
            e.HasKey(x => x.Id);
            e.Property(x => x.Name).HasMaxLength(128).IsRequired();
            e.HasOne<Folder>().WithMany().HasForeignKey(x => x.ParentId);
            e.HasOne<RecycleItem>().WithMany().HasForeignKey(x => x.RecycleItemId);
        });

        b.Entity<FolderUserPermission>(e =>
        {
            e.HasKey(x => new { x.FolderId, x.UserId });
            e.HasOne<Folder>().WithMany().HasForeignKey(x => x.FolderId);
            e.HasOne<User>().WithMany().HasForeignKey(x => x.UserId);
        });

        b.Entity<FolderGroupPermission>(e =>
        {
            e.HasKey(x => new { x.FolderId, x.GroupId });
            e.HasOne<Folder>().WithMany().HasForeignKey(x => x.FolderId);
            e.HasOne<Group>().WithMany().HasForeignKey(x => x.GroupId);
        });
    }
}
