create table CompanyGroupMaps
   ( CompanyId int not null primary key references Companies(Id)
   , GroupId int not null primary key references Groups(Id)
   );

