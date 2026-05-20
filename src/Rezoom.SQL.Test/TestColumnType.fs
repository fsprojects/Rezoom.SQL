module Rezoom.SQL.Test.ColumnTypes
open NUnit.Framework
open Rezoom.SQL.Mapping
open System

[<Test>]
let ``ColumnType.clrType is total over every enum value`` () =
    for v in Enum.GetValues(typeof<ColumnType>) do
        let c = v :?> ColumnType
        try
            ColumnType.clrType c |> ignore
        with
        | exn ->
            Assert.Fail(sprintf "ColumnType.clrType threw on %O: %s" c exn.Message)

[<Test>]
let ``ColumnType.isPrimitiveClrType recognizes every concrete primitive`` () =
    for v in Enum.GetValues(typeof<ColumnType>) do
        let c = v :?> ColumnType
        match c with
        | ColumnType.Invalid | ColumnType.Object -> ()
        | concrete ->
            let ty = ColumnType.clrType concrete
            Assert.IsTrue
                ( ColumnType.isPrimitiveClrType ty
                , sprintf "isPrimitiveClrType returned false for %O (CLR type %s)" concrete ty.FullName
                )
