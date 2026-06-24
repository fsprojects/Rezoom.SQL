create table Users
( Id int primary key autoincrement
, Name string(80)
, Email string(254)
);

create table Articles
( Id int64 primary key autoincrement
, AuthorId int64 references Users(Id)
, ArticleTitle string(80)
, ArticleText string(4096)
);
