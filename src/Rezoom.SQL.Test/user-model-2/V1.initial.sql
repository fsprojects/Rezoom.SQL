create table Users
   ( Id int primary key
   , Name string(128) null
   , Email string(128) null
   , Password binary(64) null
   , Salt binary(64) null
   );

