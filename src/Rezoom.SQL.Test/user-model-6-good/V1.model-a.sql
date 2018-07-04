create table Foos
( Id int primary key autoincrement
, SlideId int references Slides(Id)
);
