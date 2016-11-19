create table Users
   ( Id int not null primary key
   , Name string(128)
   , Email string(128)
   , Password binary(64)
   , Salt binary(64)
   );

