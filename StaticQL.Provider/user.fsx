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

type Query = SQL<"""
    select * from Users u where u.id = @id
""", "user-migrations">

let q : Command<_> = Query.Command(id = 1)
printfn "%O" <| q.GetType()
printfn "%A" <| q.Fragments
