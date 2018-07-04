create table Groups
( Id int primary key autoincrement
, Name string(64)
);

create table UserGroupMaps
( UserId int references Users(Id)
, GroupId int references Groups(Id)
, primary key (UserId, GroupId)
);

create view VUsers as select * from Users;
