module TypeProviderUser.SQLite.AssemblyInfo

open System.Reflection
open System.Runtime.InteropServices
open NUnit.Framework

[<assembly: AssemblyTitle("TypeProviderUser.SQLite")>]
[<assembly: AssemblyProduct("TypeProviderUser.SQLite")>]
[<assembly: ComVisible(false)>]
// Tests share rzsql.db; running them in parallel races on the file handle.
[<assembly: Parallelizable(ParallelScope.None)>]
[<assembly: LevelOfParallelism(1)>]
do ()
