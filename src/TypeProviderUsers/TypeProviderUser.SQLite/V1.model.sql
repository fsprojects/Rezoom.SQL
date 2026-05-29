create table Pictures
( SHA256 binary(32) primary key
, PNGData binary(4096)
);

create table Users
( Id UserId primary key autoincrement
, Name string(80)
, Email string(254)
, ProfilePictureSHA256 binary(32) null references Pictures(SHA256)
, Created datetime
, RandomId guid default(cast(randomblob(16) as guid))
, FavoriteTimeOfDay TimeOnly default('00:00:00.0000000')
, BedtimeIfAny TimeOnly null
, SignupDate DateOnly null
);

create table Articles
( Id int64 primary key autoincrement
, AuthorId int64 references Users(Id)
, ArticleTitle string(80)
, ArticleText string(4096)
);

create index IX_Articles_AuthorId on Articles(AuthorId);

create table ArticleComments
( Id int64 primary key autoincrement
, ArticleId int64 references Articles(Id)
, AuthorId int64 references Users(Id)
, CommentText string(512)
);

create index IX_ArticleComments_AuthorId on ArticleComments(AuthorId);

