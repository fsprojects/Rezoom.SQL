
create table Users
    ( Id int primary key autoincrement
    , Email string(254) unique
    , Name string(64) null
    );

create table Comments
    ( Id int primary key autoincrement
    , AuthorId int references Users(Id)
    , Comment string(512)
    );

create index IX_Comments_AuthorId on Comments
    (AuthorId);


create table Kingdoms
	( Id int primary key autoincrement
	, Name string(80)
	);

create table Phyla
	( Id int primary key autoincrement
	, KingdomId int references Kingdoms(Id)
	, Name string(80)
	);

create table Classes
	( Id int primary key autoincrement
	, PhylumId int references Phyla(Id)
	, Name string(80)
	);

create table Orders
	( Id int primary key autoincrement
	, ClassId int references Classes(Id)
	, Name string(80)
	);

create table Families
	( Id int primary key autoincrement
	, OrderId int references Orders(Id)
	, Name string(80)
	);

create table Genera
	( Id int primary key autoincrement
	, FamilyId int references Families(Id)
	, Name string(80)
	);

create table Species
	( Id int primary key autoincrement
	, GenusId int references Genera(Id)
	, Name string(80)
	);