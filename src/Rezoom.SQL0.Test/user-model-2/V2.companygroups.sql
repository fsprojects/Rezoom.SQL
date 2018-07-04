create table CompanyGroupMaps
   ( CompanyId int references Companies(Id)
   , GroupId int references Groups(Id)
   , primary key(CompanyId, GroupId)
   );

