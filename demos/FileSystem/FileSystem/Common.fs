// This file defines common types that every layer can know about.
namespace FileSystem
open System.Security

// Strongly typed wrappers for integer IDs.
// A must-have when we're doing all our domain logic by passing around IDs.

type UserId = UserId of int
type FileId = FileId of int
type FolderId = FolderId of int
type GroupId = GroupId of int
type RecycleItemId = RecycleItemId of int

/// Represents a permission that can be assigned at a level of the hierarchy.
/// This level of the hiearchy doesn't have to specify the permission, it can
/// just punt to another layer (via Inherit).
type LocalPermission =
    /// Allow the action.
    | Allow
    /// Disallow the action.
    | Deny
    /// Do not specify this permission, just inherit it from the parent folder.
    | Inherit

/// Identifies a user-like entity permissions can be controlled for.
/// Currently this is either a user or group.
type PermissionSubjectId =
    | SubjectUser of UserId
    | SubjectGroup of GroupId

/// Represents the permissions local to a folder and user or group ID.
/// In other words, a single record from the FolderPermissions table.
type LocalPermissions =
    {   DeletePermission : LocalPermission
        CreatePermission : LocalPermission
    }
    static member Empty =
        {   DeletePermission = Inherit
            CreatePermission = Inherit
        }

/// Represents a permission that has been resolved taking into account inheritance.
/// After all, the bottom line is you either can or can't do it.
type EffectivePermission =
    | Allowed
    | Denied

type EffectivePermissions =
    {   /// The folder where these effective permissions apply.
        FolderId : FolderId option
        /// The user these effective permissions apply to. We always resolve effective permissions for a specific
        /// user, taking into account all the groups they are in. There is no point in resolving effective permissions
        /// for a group because groups do not perform actions in the system, users do.
        UserId : UserId
        Delete : EffectivePermission
        Create : EffectivePermission
    }
    member this.AssertCanDelete() =
        if this.Delete <> Allowed then raise <| SecurityException("Not allowed to delete")
    member this.AssertCanCreate() =
        if this.Create <> Allowed then raise <| SecurityException("Not allowed to create")

// The below types are basically DTOs. It might make sense to put them in the persistence layer instead,
// but since they're just dumb immutable records it doesn't really hurt anything to have them shared.

type FileData =
    {   FileId : FileId
        ParentId : FolderId
        Name : string
    }

type FolderData =
    {   FolderId : FolderId
        ParentId : FolderId option // the root folder has no parent
        Name : string
    }

type FileOrFolder = 
    | Folder of FolderData
    | File of FileData
