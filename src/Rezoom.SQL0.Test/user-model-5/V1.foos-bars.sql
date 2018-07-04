create table Bars
( Id int primary key autoincrement
, FooName string(16) references Foos(Name)
);
