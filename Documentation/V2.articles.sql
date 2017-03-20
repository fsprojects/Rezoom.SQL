create table Articles
	( Id int primary key autoincrement
	, AuthorId int references Users(Id)
	, Title string(128)
	, Content string(2048)
	);

alter table Comments
	add column ArticleId int null references Articles(Id);

create index IX_Comments_ArticleId on Comments
    (ArticleId);