module FileSystem.Domain
open Rezoom
open FileSystem

/// Direct wrapper around persistence layer -- no extra logic needed here.
let getUserByName name = Persistence.getUserByName name

let rec getHierarchy rootId =
    plan {
        let! children = Persistence.getChildren rootId
        return!
            [ for child in children ->
                plan {
                    let! children =
                        match child with
                        | File _ -> Plan.ret []
                        | Folder f -> getHierarchy (Some f.FolderId) // recurse
                    return { Info = (); Node = child; Children = children }
                }
            ] |> Plan.concurrentList
    }

/// The rule we use to combine local permissions across the groups a user is in.
/// In short, if ANY group you are in explicitly denies a permission, it's denied.
let private denyWins left right =
    match left, right with
    | Allow, Allow -> Allow
    | Deny, _
    | _, Deny -> Deny
    | Inherit, x
    | x, Inherit -> x

/// The rule we use to combine local permissions betwene parent and child folders.
/// In short, the child permission is used unless it is `Inherit`.
let private childWins parent child =
    match parent, child with
    | parent, Inherit -> parent
    | _, child -> child

/// Combine two `LocalPermission`s based on a rule that takes the parent and child.
let private combineBy rule parent child =
    {   CreatePermission = rule parent.CreatePermission child.CreatePermission
        DeletePermission = rule parent.DeletePermission child.DeletePermission
    }

/// Convert a `LocalPermission` to an `EffectivePermission` based on its parent `EffectivePermission`.
let private effective parent local =
    match local with
    | Inherit -> parent
    // If the local permission is not `Inherit`, the parent permission is ignored.
    | Deny -> Denied
    | Allow -> Allowed

let private getCombinedGroupPermissions folderId groupIds =
    plan {
        let! groupPermissions =
            [ for groupId in groupIds ->
                Persistence.getLocalPermissions folderId (SubjectGroup groupId)
            ] |> Plan.concurrentList
        return
            groupPermissions
            |> List.fold (combineBy denyWins) LocalPermissions.Empty
    }

/// Without any permission records in the database, everything is denied.
let private defaultPermission = Denied

let private rootPermissions userId =
    {   FolderId = None
        UserId = userId
        Delete = defaultPermission
        Create = defaultPermission
    }

let rec getEffectivePermissions userId folderId : EffectivePermissions Plan =
    plan {
        let! folder, groupIds =
            Persistence.getFolder folderId, Persistence.getUserGroupIds userId
        let! parentPermissions =
            match folder.ParentId with
            | None -> Plan.ret (rootPermissions userId)
            | Some parentId -> getEffectivePermissions userId parentId
        let! userPermissions, groupPermissions =
             ( Persistence.getLocalPermissions folderId (SubjectUser userId)
             , getCombinedGroupPermissions folderId groupIds
             )

        // if we have both permissions from our group and permissions specifically for us, the ones
        // applied to us take priority.
        let localPermissions =
            combineBy childWins groupPermissions userPermissions

        // if we have both parent permissions and permissions specifically for this child folder,
        // the ones applied to the child folder take priority.
        return
            {   FolderId = Some folderId
                UserId = userId
                Create = effective parentPermissions.Create localPermissions.CreatePermission
                Delete = effective parentPermissions.Delete localPermissions.DeletePermission 
            }
    }

let recycleFile userId fileId =
    plan {
        let! self = Persistence.getFile fileId
        let! permissions = getEffectivePermissions userId self.ParentId
        permissions.AssertCanDelete()

        let! recycleId = Persistence.addRecycleItem userId
        do! Persistence.setFileRecycleId fileId (Some recycleId)
    }

let rec recycleFolder userId folderId =
    plan {
        let! self = Persistence.getFolder folderId
        match self.ParentId with
        | None -> invalidOp "Cannot recycle the root folder!"
        | Some parentId ->
            let! permissions = getEffectivePermissions userId parentId
            permissions.AssertCanDelete()

        let! children = Persistence.getChildren (Some folderId)
        for child in batch children do
            match child with
            | File file ->
                do! recycleFile userId file.FileId
            | Folder folder ->
                do! recycleFolder userId folder.FolderId

        let! recycleId = Persistence.addRecycleItem userId
        do! Persistence.setFolderRecycleId folderId (Some recycleId)
    }

