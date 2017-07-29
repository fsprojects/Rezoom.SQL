create table Users
   ( Id int primary key autoincrement
   , Name string(128) null
   , Email string(128)
   , Password binary(64) null
   , Salt binary(64) null
   );

create table Groups
   ( Id int primary key autoincrement
   , Name string(128) null
   );

create table UserGroupMaps
   ( UserId int references Users(Id)
   , GroupId int references Groups(Id)
   , primary key (UserId, GroupId)
   );

