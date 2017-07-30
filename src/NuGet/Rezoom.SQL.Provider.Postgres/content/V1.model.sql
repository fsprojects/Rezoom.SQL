-- Edit this file to define your initial database model.

create table Users
    ( Id int primary key autoincrement
    , Email string(254) unique
    , Name string(64) null
    );

create table Comments
    ( Id int primary key autoincrement
    , AuthorId int references Users(Id)
    , Comment string(512)
    );

create index IX_Comments_AuthorId on Comments
    (AuthorId);

-- If you want to make model changes after you have created data, add
-- the statements for those changes (e.g. ALTER TABLE) in another file
-- called V2.yourchangetitle.sql.
