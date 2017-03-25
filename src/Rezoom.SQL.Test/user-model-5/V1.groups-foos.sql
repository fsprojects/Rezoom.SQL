create table Foos
( Name string(16) primary key
, GroupId int references Groups(Id)
);
