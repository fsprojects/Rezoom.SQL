module FileSystem.Persistence
open Rezoom
open FileSystem

/// Migrate the database.
val public migrate : unit -> unit

/// Get data about a single file.
val public getFile : id : FileId -> FileData Plan

/// Get data about a single folder.
val public getFolder : id:  FolderId -> FolderData Plan

/// Get data about all the children of a folder.
val public getChildren : parentId : FolderId -> FileOrFolder list Plan

/// Get the IDs of the groups a user is a member of.
val public getUserGroupIds : userId : UserId -> GroupId list Plan

/// Get the data from the permissions record associated with a given folder and permission subject.
/// If it does not exist, returns `LocalPermissions.Empty(folderId, subjectId)`.
val public getLocalPermissions
    : folderId : FolderId -> subjectId : PermissionSubjectId -> LocalPermissions Plan

/// Add a recycle item to the persistence store and return its ID.
val public addRecycleItem : recycler : UserId -> RecycleItemId Plan

/// Set the recycle item ID of a folder.
val public setFolderRecycleId : folderId : FolderId -> recycleId : RecycleItemId option -> unit Plan

/// Set the recycle item ID of a file.
val public setFileRecycleId : fileId : FileId -> recycleId : RecycleItemId option -> unit Plan
