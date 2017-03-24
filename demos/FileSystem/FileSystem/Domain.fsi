module FileSystem.Domain
open Rezoom
open FileSystem

/// Get effective permissions for a user within a given folder.
val public getEffectivePermissions : userId : UserId -> folderId : FolderId -> EffectivePermissions Plan

/// Recycle a file.
val public recycleFile : userId : UserId -> fileId : FileId -> unit Plan

/// Recycle a folder, including its descendants.
val public recycleFolder : userId : UserId -> folderId : FolderId -> unit Plan