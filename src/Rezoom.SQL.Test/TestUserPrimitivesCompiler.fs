module Rezoom.SQL.Test.TestUserPrimitivesCompiler
open System
open System.IO
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Compiler
open Rezoom.SQL.Mapping
open Rezoom.SQL.Migrations

[<Test>]
let ``user model with custom primitives loads`` () =
    let model = userModelByName "user-model-7-usertypes"
    printfn "%d" model.UserTypeLibrary.CountPrimitives