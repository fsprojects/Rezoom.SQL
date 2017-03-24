module FileSystem.Domain
open Rezoom
open FileSystem

/// Get user ID by name.
val public getUserByName : name : string -> UserId option Plan

/// Get a hierarchy of folders.
val public getHierarchy : rootId : FolderId option -> unit Hierarchy list Plan

/// Get effective permissions for a user within a given folder.
val public getEffectivePermissions : userId : UserId -> folderId : FolderId -> EffectivePermissions Plan

/// Recycle a file.
val public recycleFile : userId : UserId -> fileId : FileId -> unit Plan

/// Recycle a folder, including its descendants.
val public recycleFolder : userId : UserId -> folderId : FolderId -> unit Plan