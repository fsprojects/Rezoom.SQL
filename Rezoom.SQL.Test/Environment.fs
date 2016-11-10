[<AutoOpen>]
module Rezoom.SQL.Test.Environment
open System
open System.Reflection
open System.IO
open Rezoom.SQL

let userModel1() =
    let assemblyFolder = Path.GetDirectoryName(Uri(Assembly.GetExecutingAssembly().CodeBase).LocalPath)
    let resolutionFolder = Path.Combine(assemblyFolder, "../../user-model-1")
    UserModel.Load(resolutionFolder, ".")

