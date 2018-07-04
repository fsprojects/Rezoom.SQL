create table Groups
   ( Id int primary key
   , Name string(128) null
   );

create table UserGroupMaps
   ( UserId int references Users(Id)
   , GroupId int references Groups(Id)
   , primary key (UserId, GroupId)
   );

