# Rezoom.SQL.Provider

The F# type provider for Rezoom.SQL. Not a massive amount of code, but it's confusing because writing type providers is hard.

This is a generative type provider, not erased. It outputs real .NET classes that can be also be consumed from C#/VB code.

The heavy lifting is done by Rezoom.SQL.Compiler (compile-time) and Rezoom.SQL.Mapping (runtime)
so this just generates the row types and some thin wrapper classes around the runtime API.