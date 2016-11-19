create table Companies
   ( Id int not null primary key
   , Name string(128)
   );

alter table Users
add column EmployerId int
references Companies(Id);
