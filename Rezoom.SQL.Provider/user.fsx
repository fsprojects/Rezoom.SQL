#I "bin/Debug"
#r "FParsec.dll"
#r "FParsecCS.dll"
#r "FParsec-Pipes.dll"
#r "LicenseToCIL.dll"
#r "Rezoom.dll"
#r "Rezoom.SQL.Compiler.dll"
#r "Rezoom.SQL.Mapping.dll"
#r "Rezoom.SQL.Provider.dll"

open Rezoom.SQL.Provider
open Rezoom.SQL.Mapping

type M = SQLModel

type Query = SQL<"""
    select * from Users u where u.id = @id
""", "user-migrations">

type QueryInPar = SQL<"""
    select * from Users u where u.id in @id
""", "user-migrations">

type QueryWithNullablePar = SQL<"""
    select * from Users u
    where u.Name is @name
""", "user-migrations">

try
  let q : Command<_> = Query.Command(id = 1)
  printfn "%O" <| q.GetType()
  printfn "%A" <| q.Fragments

  let qIn : Command<_> = QueryInPar.Command(id = [|1|])
  printfn "%O" <| qIn.GetType()
  printfn "%A" <| qIn.Fragments

  let qNull : Command<_> = QueryWithNullablePar.Command(name = Some "test")
  printfn "%O" <| qNull.GetType()
  printfn "%A" <| qNull.Fragments
with
| ex ->
  printfn "%O" ex
  ignore <| System.Console.ReadLine()
