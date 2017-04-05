module Rezoom.SQL.Test.TestVendorStatements
open System
open NUnit.Framework
open FsUnit
open Rezoom.SQL.Compiler
open Rezoom.SQL.Mapping

let normalizeFragments fragments =
    fragments
    |> Seq.map (function
        | CommandText t -> CommandText <| t.Replace("\r\n", "\n")
        | x -> x)
    |> List.ofSeq

let vendor (sql : string) expected =
    let userModel = userModel1()
    let parsed = CommandEffect.OfSQL(userModel.Model, "anonymous", sql)
    let indexer = dispenserParameterIndexer()
    let fragments =
        userModel.Backend.ToCommandFragments(indexer, parsed.Statements)
        |> normalizeFragments
    printfn "%A" fragments
    if fragments <> normalizeFragments expected then
        failwith "Mismatch"

[<Test>]
let ``vendor without exprs or imaginary`` () =
    vendor """
        vendor sqlite {
            this is raw text
        }
    """
        [   CommandText "
            this is raw text
        ;"
            LineBreak
        ]

[<Test>]
let ``vendor without imaginary`` () =
    vendor """
        vendor sqlite {
            raw text {@param1} more raw {@param2}
        }
    """
        [   CommandText "
            raw text "
            Parameter 0
            CommandText " more raw "
            Parameter 1
            CommandText "
        ;"
            LineBreak
        ]

[<Test>]
let ``vendor with imaginary`` () =
    vendor """
        vendor sqlite {
            raw text {@param1} more raw {@param2}
        } imagine {
            select Id from Users
        }
    """
        [   CommandText "
            raw text "
            Parameter 0
            CommandText " more raw "
            Parameter 1
            CommandText "
        ;"
            LineBreak
        ]

[<Test>]
let ``vendor with wacky delimiters`` () =
    vendor """
        vendor sqlite [:<#
            raw text [:<# @param1 #>:] more raw [:<# @param2 #>:]
        #>:] imagine [:<#
            select Id from Users
        #>:]
    """
        [   CommandText "
            raw text "
            Parameter 0
            CommandText " more raw "
            Parameter 1
            CommandText "
        ;"
            LineBreak
        ]
