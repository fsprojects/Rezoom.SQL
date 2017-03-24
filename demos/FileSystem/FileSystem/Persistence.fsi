// This file defines the public interface to the persistence layer.

// In theory we could implement this set of functions against something other than a SQL database,
// including in memory for testing.

module FileSystem.Persistence
open Rezoom
open FileSystem

/// Get a user ID by their name.
val public getUserByName : name : string -> UserId option Plan

/// Get data about a single file.
val public getFile : id : FileId -> FileData Plan

/// Get data about a single folder.
val public getFolder : id : FolderId -> FolderData Plan

/// Get data for the non-recycled children of a folder.
val public getChildren : parentId : FolderId option -> FileOrFolder list Plan

/// Get the IDs of the groups a user is a member of.
val public getUserGroupIds : userId : UserId -> GroupId list Plan

/// Get the data from the permissions record associated with a given folder and permission subject.
/// If no permissions are specified for the particular combination, returns `LocalPermissions.Empty.
val public getLocalPermissions
    : folderId : FolderId -> subjectId : PermissionSubjectId -> LocalPermissions Plan

/// Add a recycle item to the persistence store and return its ID.
val public addRecycleItem : recycler : UserId -> RecycleItemId Plan

/// Set the recycle item ID of a folder.
val public setFolderRecycleId : folderId : FolderId -> recycleId : RecycleItemId option -> unit Plan

/// Set the recycle item ID of a file.
val public setFileRecycleId : fileId : FileId -> recycleId : RecycleItemId option -> unit Plan
