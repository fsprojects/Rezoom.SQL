create table Pictures
( SHA256 binary(32) primary key
, PNGData binary(4096)
);

create table Users
( Id UserId primary key autoincrement
, Name string(80)
, Email string(254)
, ProfilePictureSHA256 binary(32) null references Pictures(SHA256)
);

create table Articles
( Id int64 primary key autoincrement
, AuthorId int64 references Users(Id)
, ArticleTitle string(80)
, ArticleText string(4096)
);
