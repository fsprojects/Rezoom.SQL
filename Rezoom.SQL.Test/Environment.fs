[<AutoOpen>]
module Rezoom.SQL.Test.Environment
open NUnit.Framework
open FsUnit
open System
open System.Reflection
open System.IO
open Rezoom.SQL

let userModel1() =
    let assemblyFolder = Path.GetDirectoryName(Uri(Assembly.GetExecutingAssembly().CodeBase).LocalPath)
    let resolutionFolder = Path.Combine(assemblyFolder, "../../user-model-1")
    UserModel.Load(resolutionFolder, ".")

let expectError (msg : string) (sql : string) =
    let userModel = userModel1()
    try
        ignore <| CommandEffect.OfSQL(userModel.Model, "anonymous", sql)
        failwith "Should've thrown an exception!"
    with
    | :? SourceException as exn ->
        printfn "\"%s\"" exn.Message
        Assert.AreEqual(msg, exn.Reason)

