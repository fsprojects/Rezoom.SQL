namespace Rezoom.ORM.SQLProvider
open System
open System.Collections.Generic
open System.Globalization
open SQLow
open Rezoom.ORM
open Rezoom.ORM.SQLProvider.BackendUtilities

type SQLiteLiteral() =
    inherit LiteralTranslator()
    override __.BlobLiteral(bytes) =
        let hexPairs = bytes |> Array.map (fun b -> b.ToString("X2", CultureInfo.InvariantCulture))
        "x'" + String.Concat(hexPairs) + "'"
        |> text
    override __.StringLiteral(str) =
        "'" + str.Replace("'", "''") + "'"
        |> text

type SQLiteExpression(statement : StatementTranslator, indexer) =
    inherit ExprTranslator(statement, indexer)
    let literal = SQLiteLiteral()
    override __.Name(name) =
        "\"" + name.Value.Replace("\"", "\"\"") + "\""
        |> text
    override __.Literal = upcast literal
    override __.TypeName(name) =
        (Seq.singleton << text) <|
            match name with
            | BooleanTypeName
            | IntegerTypeName Integer8
            | IntegerTypeName Integer16
            | IntegerTypeName Integer32
            | IntegerTypeName Integer64 -> "int"
            | FloatTypeName Float32
            | FloatTypeName Float64 -> "float"
            | StringTypeName(_) -> "varchar"
            | BinaryTypeName(_) -> "blob"
            | DecimalTypeName
            | DateTimeTypeName
            | DateTimeOffsetTypeName -> failwith <| sprintf "Unsupported type ``%A``" name

type SQLiteStatement(indexer : IParameterIndexer) as this =
    inherit StatementTranslator()
    let expr = SQLiteExpression(this :> StatementTranslator, indexer)
    override __.Expr = upcast expr

type SQLiteBackend() =
    static let initialModel =
        let main, temp = Name("main"), Name("temp")
        {   Schemas =
                [   {   SchemaName = main
                        Tables = Map.empty
                        Views = Map.empty
                    }
                    {   SchemaName = temp
                        Tables = Map.empty
                        Views = Map.empty
                    }
                ] |> List.map (fun s -> s.SchemaName, s) |> Map.ofList
            DefaultSchema = main
            TemporarySchema = temp
            Builtin =
                {   Functions = Map.empty
                }
        }
    interface IBackend with
        member this.InitialModel = initialModel
        member this.ToCommandFragments(indexer, stmts) =
            let translator = SQLiteStatement(indexer)
            translator.Statements(stmts)
            |> BackendUtilities.simplifyFragments
            |> ResizeArray
            :> _ IReadOnlyList
       