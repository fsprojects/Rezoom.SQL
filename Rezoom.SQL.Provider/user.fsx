#I "bin/Debug"
#r "FSharp.Core.dll"
#r "FParsec.dll"
#r "FParsecCS.dll"
#r "FParsec-Pipes.dll"
#r "LicenseToCIL.dll"
#r "Rezoom.SQL.dll"
#r "Rezoom.SQL.Mapping.dll"
#r "Rezoom.SQL.Provider.dll"

open Rezoom.SQL.Provider
open Rezoom.SQL.Mapping

type M = Model

type Query = SQL<"""
    select * from Users u where u.id = @id
""", "user-migrations">

let q : Command<_> = Query.Command(id = 1)
printfn "%O" <| q.GetType()
printfn "%A" <| q.Fragments
