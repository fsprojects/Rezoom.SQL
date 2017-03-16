(*** hide ***)

#r "../packages/Rezoom.0.2.1/lib/net46/Rezoom.dll"
#r "../packages/Rezoom.SQL.Provider.0.1.2/lib/net46/Rezoom.SQL.Compiler.dll"
#r "../packages/Rezoom.SQL.Provider.0.1.2/lib/net46/Rezoom.SQL.Mapping.dll"
#r "../packages/Rezoom.SQL.Provider.0.1.2/lib/net46/Rezoom.SQL.Provider.dll"
#nowarn "193"
open System

(**

# Tutorial

This tutorial will get you up and running with Rezoom.SQL. You'll need
[Visual Studio 2017](https://www.visualstudio.com/downloads/) -- the free
Community Edition is fine.

Make sure you check the box for "F# language support" in the installer.
If you've already installed VS2017, you can re-run the installer and modify
your installation to include F# language support.

*)

open Rezoom.SQL.Provider
open Rezoom.SQL.Synchronous

type Q = SQL<"""
    create table X(a int primary key autoincrement, b string(10));
    select * from X;
""">

let xs = Q.Command().Execute(null)
for x in xs do
    printfn "%d %s" y.a y.b

