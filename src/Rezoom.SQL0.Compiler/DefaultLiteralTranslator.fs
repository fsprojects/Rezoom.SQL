namespace Rezoom.SQL.Compiler.Translators
open System
open System.Globalization
open Rezoom.SQL.Compiler
open Rezoom.SQL.Compiler.BackendUtilities
open Rezoom.SQL.Mapping

type DefaultLiteralTranslator() =
    inherit LiteralTranslator()
    override __.NullLiteral = CommandText "NULL"
    override __.BooleanLiteral b = CommandText <| if b then "TRUE" else "FALSE"
    override __.IntegerLiteral i = CommandText (i.ToString(CultureInfo.InvariantCulture))
    override __.FloatLiteral f = CommandText (f.ToString("0.0##############", CultureInfo.InvariantCulture))
    override __.BlobLiteral(bytes) =
        let hexPairs = bytes |> Array.map (fun b -> b.ToString("X2", CultureInfo.InvariantCulture))
        "x'" + String.Concat(hexPairs) + "'"
        |> text
    override __.StringLiteral(str) =
        "'" + str.Replace("'", "''") + "'"
        |> text
    override __.DateTimeLiteral(dt) =
        CommandText <| dt.ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fff")
    override __.DateTimeOffsetLiteral(dt) =
        CommandText <| dt.ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fffzzz")
    override this.Literal literal =
        match literal with
        | NullLiteral -> this.NullLiteral
        | BooleanLiteral t -> this.BooleanLiteral(t)
        | StringLiteral str -> this.StringLiteral(str)
        | BlobLiteral blob -> this.BlobLiteral(blob)
        | DateTimeLiteral dt -> this.DateTimeLiteral(dt)
        | DateTimeOffsetLiteral dt -> this.DateTimeOffsetLiteral(dt)
        | NumericLiteral (IntegerLiteral i) -> this.IntegerLiteral(i)
        | NumericLiteral (FloatLiteral f) -> this.FloatLiteral(f)
    override this.SignedLiteral literal =
        let literalValue = literal.Value |> NumericLiteral |> this.Literal
        if literal.Sign >= 0 then Seq.singleton literalValue else
        seq {
            yield text "-"
            yield literalValue
        }

