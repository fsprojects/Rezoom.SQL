create table Comments
( Id int64 primary key autoincrement
, AuthorId int references Users(Id)
, Text string(512)
);

create view VComments as select * from Users;
