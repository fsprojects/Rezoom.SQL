create table ToDos
	( Id int primary key -- TODO: support autoincrement
	, ParentId int null references ToDos(Id)
	, Heading string(256)
	, Paragraph string(512) null
	, DeactivatedUtc string(64) null
	);

create view ActiveToDos as
	select * from ToDos where DeactivatedUtc is null;