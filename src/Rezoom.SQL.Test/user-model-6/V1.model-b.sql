create table Bars
( Id int primary key autoincrement
, FooId int references Foos(Id)
);
