-- can link comments and groups
create table Baz
( CommentId int references Comments(Id)
, GroupId int references Groups(Id)
, primary key (CommentId, GroupId)
);
