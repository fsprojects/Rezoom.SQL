#I "bin/Debug"
#r "FSharp.Core.dll"
#r "FParsec.dll"
#r "FParsecCS.dll"
#r "FParsec-Pipes.dll"
#r "LicenseToCIL.dll"
#r "SQLow.dll"
#r "Rezoom.ORM.SQLProvider.dll"

open Rezoom.ORM.SQLProvider
type Query = SQL<"foo", """
create table Users (id int primary key, name nvarchar(30));
select * from Users
""">

let q = Query()
