create table Fiddles
( SHA1 binary(20) primary key
, CreatedUtc datetime
, Backend string(8)
, Model string
, Command string
);
