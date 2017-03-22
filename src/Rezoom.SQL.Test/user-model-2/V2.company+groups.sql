create table CompanyGroupMaps
   ( CompanyId int primary key references Companies(Id)
   , GroupId int primary key references Groups(Id)
   );

