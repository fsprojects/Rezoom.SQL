create table Users
   ( Id int primary key
   , Name string(128) null
   , Email string(128) null
   , Password binary(64) null
   , Salt binary(64) null
   );

create table Groups
   ( Id int primary key
   , Name string(128) null
   );

create table UserGroupMaps
   ( UserId int primary key references Users(Id)
   , GroupId int primary key references Groups(Id)
   );

