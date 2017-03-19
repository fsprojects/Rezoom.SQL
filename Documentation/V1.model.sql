
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