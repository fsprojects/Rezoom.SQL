module Rezoom.SQL.Test.TestTSQL
open System
open NUnit.Framework
open FsUnit
open Rezoom.SQL
open Rezoom.SQL.Mapping

let translate (sql : string) (expectedTSQL : string) =
    let userModel =
        let userModel = userModel1()
        let backend = TSQL.TSQLBackend() :> IBackend
        { userModel with
            Backend = backend
            Model = { userModel.Model with Builtin = backend.InitialModel.Builtin }
        }
    let parsed = CommandEffect.OfSQL(userModel.Model, "anonymous", sql)
    let indexer = { new IParameterIndexer with member __.ParameterIndex(_) = 0 }
    let fragments = userModel.Backend.ToCommandFragments(indexer, parsed.Statements)
    let str = CommandFragment.Stringize(fragments)
    Console.WriteLine(str)
    Assert.AreEqual(expectedTSQL, str)

[<Test>]
let ``at at proc translation`` () =
    translate
        """select datefirst() as d"""
        """SELECT @@DATEFIRST AS [d];"""

[<Test>]
let ``datepart translation`` () =
    translate
        """select dateadd('day', 1, sysutcdatetime()) d"""
        """SELECT dateadd(day,1,sysutcdatetime()) AS [d];"""

