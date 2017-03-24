module FileSystem.Persistence
open Rezoom
open Rezoom.SQL
open Rezoom.SQL.Migrations
open Rezoom.SQL.Plans

/// Model inferred from V1.model.sql.
type private FileSystemModel = SQLModel<".">

let migrate() =
    let config =
        {   AllowRetroactiveMigrations = false
            LogMigrationRan = fun m -> printfn "Ran migration `%s`" m.FileName
        }
    FileSystemModel.Migrate(config)

type private GetFileSQL = SQL<"""
    select Id, Name, ParentId from Files where Id = @id
""">

let getFile (FileId fileId) =
    plan {
        let! row = GetFileSQL.Command(fileId).ExactlyOne()
        return
            {   FileId = FileId row.Id
                ParentId = FolderId row.ParentId
                Name = row.Name
            }
    }

type private GetFolderSQL = SQL<"""
    select Id, Name, ParentId from Folders where Id = @id
""">

let getFolder (FolderId folderId) =
    plan {
        let! row = GetFolderSQL.Command(folderId).ExactlyOne()
        return
            {   FolderId = FolderId row.Id
                ParentId = Option.map FolderId row.ParentId
                Name = row.Name
            }
    }

type private GetChildrenSQL = SQL<"""
    select Id, Name, true as IsFolder
    from Folders where ParentId = @parentId
    union all
    select Id, Name, false as IsFolder
    from Files where ParentId = @parentId
""">

let getChildren (FolderId parentId) =
    plan {
        let! rows = GetChildrenSQL.Command(parentId).Plan()
        let parentId = FolderId parentId
        return
            [ for row in rows ->
                if row.IsFolder then
                    {   FolderId = FolderId row.Id
                        ParentId = Some parentId
                        Name = row.Name
                    } |> Folder
                else
                    {   FileId = FileId row.Id
                        ParentId = parentId
                        Name = row.Name
                    } |> File
            ]
    }

type private GetUserGroupIdsSQL = SQL<"""
    select gm.GroupId
    from Users u
    join UserGroups gm on gm.UserId = u.Id
    where u.Id = @userId
""">

let getUserGroupIds (UserId userId) =
    plan {
        let! rows = GetUserGroupIdsSQL.Command(userId).Plan()
        return [ for row in rows -> GroupId row.GroupId ]
    }
    
type private GetLocalPermissionsSQL = SQL<"""
    select DeletePermission, CreatePermission
    from FolderPermissions
    where FolderId = @folderId
    and UserId is @userId
    and GroupId is @groupId
""">

let private permissionFromBool b =
    match b with
    | None -> Inherit
    | Some true -> Allow
    | Some false -> Deny

let getLocalPermissions (FolderId folderId) subjectId =
    plan {
        let command =
            match subjectId with
            | SubjectUser (UserId id) ->
                GetLocalPermissionsSQL.Command(folderId = folderId, userId = Some id, groupId = None)
            | SubjectGroup (GroupId id) ->
                GetLocalPermissionsSQL.Command(folderId = folderId, userId = None, groupId = Some id)
        let! row = command.TryExactlyOne()
        return
            match row with
            | None -> LocalPermissions.Empty(FolderId folderId, subjectId)
            | Some found ->
                {   FolderId = FolderId folderId
                    SubjectId = subjectId
                    DeletePermission = permissionFromBool found.DeletePermission
                    CreatePermission = permissionFromBool found.CreatePermission
                }
    }

type AddRecycleItemSQL = SQL<"""
    insert into RecycleItems(RecycledUtc, RecycledById)
    values (sysutcdatetime(), @recyclerId);

    select scope_identity() as InsertedId;
""">

let addRecycleItem (UserId recyclerId) =
    plan {
        let! insertedId = AddRecycleItemSQL.Command(recyclerId).Scalar()
        return RecycleItemId insertedId
    }

type SetFolderRecycleIdSQL = SQL<"""
    update Folders set RecycleItemId = @recycleId
    where Id = @folderId
""">

let setFolderRecycleId (FolderId folderId) recycleId =
    SetFolderRecycleIdSQL
        .Command(folderId = folderId, recycleId = (recycleId |> Option.map (fun (RecycleItemId id) -> id)))
        .Plan()

type SetFileRecycleIdSQL = SQL<"""
    update Files set RecycleItemId = @recycleId
    where Id = @fileId
""">

let setFileRecycleId (FileId fileId) recycleId =
    SetFileRecycleIdSQL
        .Command(fileId = fileId, recycleId = (recycleId |> Option.map (fun (RecycleItemId id) -> id)))
        .Plan()