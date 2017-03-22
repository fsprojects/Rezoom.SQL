create table Groups
   ( Id int primary key
   , Name string(128) null
   );

create table UserGroupMaps
   ( UserId int primary key references Users(Id)
   , GroupId int primary key references Groups(Id)
   );

