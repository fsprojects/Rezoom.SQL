module FileSystem.DemoSetup
open Rezoom
open Rezoom.SQL
open Rezoom.SQL.Migrations
open Rezoom.SQL.Plans

/// Model inferred from V1.model.sql.
type private FileSystemModel = SQLModel

let migrate() =
    let config =
        {   AllowRetroactiveMigrations = false
            LogMigrationRan = fun m -> printfn "Ran migration `%s`" m.MigrationName
        }
    FileSystemModel.Migrate(config)

type private NukeDataSQL = SQL<"""
    delete from Files;
    delete from FolderUserPermissions;
    delete from FolderGroupPermissions;
    delete from Folders;
    delete from RecycleItems;
    delete from UserGroups;
    delete from Groups;
    delete from Users;
""">

let private nukeData = NukeDataSQL.Command().Plan()

type private FolderStructureEntry =
    {   Name : string
        IsFolder : bool
        Children : FolderStructureEntry list
    }

let private file name = { Name = name; IsFolder = false; Children = [] }
let private dir name children = { Name = name; IsFolder = true; Children = children }

let private demoFolderStructure =
    [   dir "root"
            [   dir "a"
                    [   dir "a.1"
                            [   file "a.1.1"
                                file "a.1.2"
                                file "a.1.3"
                            ]
                        file "a.2"
                        file "a.3"
                        dir "a.4"
                            [   file "a.4.1"
                                file "a.4.2"
                                file "a.4.3"
                                file "a.4.5"
                            ]
                    ]
                dir "b"
                    [   dir "b.1"
                            [   file "b.1.1"
                                file "b.1.2"
                                dir "b.1.3"
                                    [   file "b.1.3.1"
                                        file "b.1.3.2"
                                        dir "b.1.3.3"
                                            [   file "b.1.3.3.1"
                                                file "b.1.3.3.2"
                                            ]
                                    ]
                            ]
                        dir "b.2"
                            [   dir "b.2.1"
                                    [   file "b.2.1.1"
                                    ]
                                file "b.2.2"
                                file "b.2.3"
                            ]
                        file "b.3"
                        file "b.4"
                        file "b.5"
                        file "b.6"
                    ]
                file "c"
                file "d"
            ]
    ]

type private InsertFolderSQL = SQL<"""
    insert into Folders(ParentId, Name)
    values (@parentId, @name);
    select scope_identity() as InsertedId;
""">

type private InsertFileSQL = SQL<"""
    insert into Files(ParentId, Name, Content)
    values (@parentId, @name, @content)
""">

let rec private setUpFolders parentId (entries : FolderStructureEntry list) =
    plan {
        for entry in batch entries do
            if entry.IsFolder then
                let! id = InsertFolderSQL.Command(entry.Name, parentId).Scalar()
                do! setUpFolders (Some id) entry.Children
            else
                let content = "This is the content of " + entry.Name
                do! InsertFileSQL.Command(content, entry.Name, Option.get parentId).Plan()
    }

type private DemoPermisssion =
    | AllowUnder of string
    | DenyUnder of string

let private demoUsers =
    [   
        "Kelly", [AllowUnder "root"]
        "Graham", [AllowUnder "a"; DenyUnder "a.3"]
        "Robert", [AllowUnder "b.1"]
        "John", [AllowUnder "b"; DenyUnder "b.1"; AllowUnder "b.1.3"]
        "Sam", []
        "Christopher", [AllowUnder "b.2.1"]
    ]

type private InsertUserSQL = SQL<"""
    insert into Users
        ( Name
        , Email
        )
    values
        ( @name
        , @email
        );
    select scope_identity() as InsertedId;
""">

type private InsertUserPermissionSQL = SQL<"""
    insert into FolderUserPermissions
        ( FolderId
        , UserId
        , DeletePermission
        , CreatePermission
        )
    select
        f.Id
        , @userId
        , @deletePermission
        , @createPermission
    from Folders f where f.Name = @folderName
""">

let rec private setupDemoUser name (permissions : DemoPermisssion list) =
    plan {
        let! userId = InsertUserSQL.Command(email = name + "@example.com", name = name).Scalar()
        for permission in batch permissions do
            let permission, folderName =
                match permission with
                | AllowUnder name -> true, name
                | DenyUnder name -> false, name
            do!
                InsertUserPermissionSQL
                    .Command(userId = userId, folderName = folderName,
                        deletePermission = Some permission,
                        createPermission = Some permission)
                    .Plan()
    }

let private demoGroups =
    [   "NotB2A4", [AllowUnder "root"; DenyUnder "b.2"; DenyUnder "a.4"], ["Sam"; "Christopher"]
    ]

type private InsertUserGroupSQL = SQL<"""
    insert into UserGroups(UserId, GroupId)
    select
        u.Id,
        @groupId
    from Users u where u.Name = @userName
""">

type private InsertGroupSQL = SQL<"""
    insert into Groups
        ( Name
        )
    values
        ( @name
        );
    select scope_identity() as InsertedId;
""">

type private InsertGroupPermissionSQL = SQL<"""
    insert into FolderGroupPermissions
        ( FolderId
        , GroupId
        , DeletePermission
        , CreatePermission
        )
    select
        f.Id
        , @groupId
        , @deletePermission
        , @createPermission
    from Folders f where f.Name = @folderName
""">

let private setupDemoGroup name permissions members =
    plan {
        let! groupId = InsertGroupSQL.Command(name = name).Scalar()
        for permission in batch permissions do
            let permission, folderName =
                match permission with
                | AllowUnder name -> true, name
                | DenyUnder name -> false, name
            do!
                InsertGroupPermissionSQL
                    .Command(groupId = groupId, folderName = folderName,
                        deletePermission = Some permission,
                        createPermission = Some permission)
                    .Plan()
        for name in batch members do
            do! InsertUserGroupSQL.Command(groupId = groupId, userName = name).Plan()
    }

let setUpDemoData =
    plan {
        do! nukeData
        do! setUpFolders None demoFolderStructure
        for demoUserName, demoUserPermissions in batch demoUsers do
            do! setupDemoUser demoUserName demoUserPermissions
        for demoGroupName, demoGroupPermissions, demoGroupMembers in batch demoGroups do
            do! setupDemoGroup demoGroupName demoGroupPermissions demoGroupMembers
    }

let defaultUserName = demoUsers |> List.head |> fst