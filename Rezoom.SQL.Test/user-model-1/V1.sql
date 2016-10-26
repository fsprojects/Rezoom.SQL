create table Users
   ( Id int not null primary key
   , Name string(128)
   , Email string(128)
   , Password binary(64)
   , Salt binary(64)
   );

create table Groups
   ( Id int not null primary key
   , Name string(128)
   );

create table UserGroupMaps
   ( UserId int not null primary key references Users(Id)
   , GroupId int not null primary key references Groups(Id)
   );

