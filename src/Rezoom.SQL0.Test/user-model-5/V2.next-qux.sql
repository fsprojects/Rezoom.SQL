create table Qux
( CommentId int references Comments(Id)
, FooName string(16) references Foos(Name)
, primary key (CommentId, FooName)
);
