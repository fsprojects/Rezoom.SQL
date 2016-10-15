namespace StaticQL.SQLite
open System
open System.Data
open System.Collections.Generic
open System.Globalization
open StaticQL
open StaticQL.Mapping
open StaticQL.BackendUtilities
open StaticQL.Translators

type private SQLiteLiteral() =
    inherit DefaultLiteralTranslator()
    override __.BooleanLiteral(t) = CommandText <| if t then "1" else "0"

type private SQLiteExpression(statement : StatementTranslator, indexer) =
    inherit DefaultExprTranslator(statement, indexer)
    let literal = SQLiteLiteral()
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

type private SQLiteStatement(indexer : IParameterIndexer) as this =
    inherit DefaultStatementTranslator(indexer)
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
        member this.ParameterTransform(columnType) = ParameterTransform.Default(columnType)
        member this.ToCommandFragments(indexer, stmts) =
            let translator = SQLiteStatement(indexer)
            translator.Statements(stmts)
            |> BackendUtilities.simplifyFragments
            |> ResizeArray
            :> _ IReadOnlyList
       