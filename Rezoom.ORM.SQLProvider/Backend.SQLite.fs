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

type SQLiteStatement(indexer : IParameterIndexer) as this =
    inherit StatementTranslator()
    let expr = SQLiteExpression(this :> StatementTranslator, indexer)
    override __.Expr = upcast expr

type SQLiteBackend() =
    interface IBackend with
        member this.Builtin =
            {   Functions = Map.empty
            }
        member this.MapPrimitiveType(ty) =
            match ty.Type with
            | IntegerType -> if ty.Nullable then typeof<Nullable<int64>> else typeof<int64>
            | BooleanType -> if ty.Nullable then typeof<Nullable<bool>> else typeof<bool>
            | FloatType -> if ty.Nullable then typeof<Nullable<double>> else typeof<double>
            | StringType -> typeof<string>
            | BlobType -> typeof<byte array>
            | AnyType -> typeof<obj>
        member this.ToCommandFragments(indexer, stmts) =
            let translator = SQLiteStatement(indexer)
            translator.Statements(stmts)
            |> BackendUtilities.simplifyFragments
            |> ResizeArray
            :> _ IReadOnlyList
       