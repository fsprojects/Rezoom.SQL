create table Users
    ( Id int primary key autoincrement
    , Email string(256) unique
    , Name string(64)
    );

create table Groups
    ( Id int primary key autoincrement
    , Name string(64) unique
    );

create table UserGroups
    ( UserId int references Users(Id)
    , GroupId int references Groups(Id)
    , primary key(UserId, GroupId)
    );

create table RecycleItems
    ( Id int primary key autoincrement
    , RecycledUtc datetime
    , RecycledById int references Users(Id)
    );

create table Folders
    ( Id int primary key autoincrement
    -- only the root folder will have ParentId = null
    , ParentId int null references Folders(Id)
    , Name string(128)
    , RecycleItemId int null references RecycleItems(Id)
    );

create table Files
    ( Id int primary key autoincrement
    , ParentId int references Folders(Id)
    , Name string(128)
    , Content binary
    , RecycleItemId int null references RecycleItems(Id)
    );

create table FolderUserPermissions
    ( FolderId int references Folders(Id)
    , UserId int references Users(Id)
    -- Permissions apply *within* the folder, not *to* it
    , DeletePermission bool null
    , CreatePermission bool null
    , primary key(FolderId, UserId)
    );

create table FolderGroupPermissions
    ( FolderId int references Folders(Id)
    , GroupId int references Groups(Id)
    -- Permissions apply *within* the folder, not *to* it
    , DeletePermission bool null
    , CreatePermission bool null
    , primary key(FolderId, GroupId)
    );

create view ActiveFolders as
	select * from Folders where RecycleItemId is null;

create view ActiveFiles as
	select * from Files where RecycleItemId is null;
