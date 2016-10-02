namespace StaticQL.Provider
open System
open System.Collections.Generic
open StaticQL.Mapping
open StaticQL

type IParameterIndexer =
    abstract member ParameterIndex : parameter : BindParameter -> int

type IBackend =
    abstract member InitialModel : Model
    abstract member ToCommandFragments
        : indexer : IParameterIndexer * stmts : TStmt IReadOnlyList -> CommandFragment IReadOnlyList
