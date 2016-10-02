#I "bin/Debug"
#r "FSharp.Core.dll"
#r "FParsec.dll"
#r "FParsecCS.dll"
#r "FParsec-Pipes.dll"
#r "LicenseToCIL.dll"
#r "StaticQL.dll"
#r "StaticQL.Mapping.dll"
#r "StaticQL.Provider.dll"

open StaticQL.Provider
open StaticQL.Mapping

type Query = SQL<"foo", """
create table Users (id int primary key not null, name nvarchar(30));
select * from Users u where u.id = @id
""">

let q : Command1<_> = Query.Command(id = 1L)
printfn "%O" <| q.GetType()
printfn "%A" <| q.Fragments
