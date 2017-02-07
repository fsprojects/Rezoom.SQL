create table Users
       ( Id int primary key
       , Name string(128) null
       , Email string(128)
       , Password binary(64)
       , Salt binary(64)
       );

create Table Groups
       ( Id int primary key
       , Name string(128)
       );

create table UserGroupMaps
       ( UserId int references Users(Id)
       , GroupId int references Groups(Id)
       , primary key(UserId, GroupId)
       );

