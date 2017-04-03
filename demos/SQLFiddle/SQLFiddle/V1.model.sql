create table Fiddles
( SHA1 binary(20) primary key
, CreatedUtc datetime default(sysutcdatetime())
, Backend string(8)
, Model string
, Command string
, Valid bool
, Deleted bool default(false)
);

create index IX_Fiddles_Deleted on Fiddles(Deleted);

create table StandardFiddles
( Id int primary key autoincrement
, Title string(128)
, SHA1 binary(20) references Fiddles(SHA1)
);