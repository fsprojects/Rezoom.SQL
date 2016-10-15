namespace StaticQL.Translators
open System
open System.Globalization
open StaticQL
open StaticQL.Mapping
open StaticQL.BackendUtilities

type DefaultLiteralTranslator() =
    inherit LiteralTranslator()
    override __.NullLiteral = CommandText "NULL"
    override __.IntegerLiteral i = CommandText (i.ToString(CultureInfo.InvariantCulture))
    override __.FloatLiteral f = CommandText (f.ToString("0.0##############", CultureInfo.InvariantCulture))
    override __.BlobLiteral(bytes) =
        let hexPairs = bytes |> Array.map (fun b -> b.ToString("X2", CultureInfo.InvariantCulture))
        "x'" + String.Concat(hexPairs) + "'"
        |> text
    override __.StringLiteral(str) =
        "'" + str.Replace("'", "''") + "'"
        |> text
    override this.Literal literal =
        match literal with
        | NullLiteral -> this.NullLiteral
        | StringLiteral str -> this.StringLiteral(str)
        | BlobLiteral blob -> this.BlobLiteral(blob)
        | NumericLiteral (IntegerLiteral i) -> this.IntegerLiteral(i)
        | NumericLiteral (FloatLiteral f) -> this.FloatLiteral(f)
    override this.SignedLiteral literal =
        let literalValue = literal.Value |> NumericLiteral |> this.Literal
        if literal.Sign >= 0 then Seq.singleton literalValue else
        seq {
            yield text "-"
            yield literalValue
        }

