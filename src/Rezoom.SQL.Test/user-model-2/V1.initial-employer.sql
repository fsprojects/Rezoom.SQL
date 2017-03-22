create table Companies
   ( Id int primary key
   , Name string(128) null
   );

alter table Users
add column EmployerId int
references Companies(Id);
