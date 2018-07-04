namespace Rezoom.SQL.Mapping
open System
open System.Data

[<AbstractClass>]
type Row() =
    abstract member IsNull : int16 -> bool
    abstract member GetObject : int16 -> obj
    abstract member GetString : int16 -> string
    abstract member GetByte : int16 -> byte
    abstract member GetInt16 : int16 -> int16
    abstract member GetInt32 : int16 -> int32
    abstract member GetInt64 : int16 -> int64
    abstract member GetSByte : int16 -> sbyte
    abstract member GetUInt16 : int16 -> uint16
    abstract member GetUInt32 : int16 -> uint32
    abstract member GetUInt64 : int16 -> uint64
    abstract member GetSingle : int16 -> single
    abstract member GetDouble : int16 -> double
    abstract member GetDecimal : int16 -> decimal
    abstract member GetDateTime : int16 -> DateTime
    abstract member GetBoolean : int16 -> bool
    abstract member GetGuid : int16 -> Guid

type ObjectRow([<ParamArray>] row : obj array) =
    inherit Row()
    override __.IsNull(i) = isNull (row.[int i]) || obj.ReferenceEquals(DBNull.Value, row.[int i])
    override __.GetObject(i) = row.[int i]
    override __.GetString(i) = row.[int i] |> Unchecked.unbox
    override __.GetByte(i) = row.[int i] |> Unchecked.unbox
    override __.GetInt16(i) = row.[int i] |> Unchecked.unbox
    override __.GetInt32(i) = row.[int i] |> Unchecked.unbox
    override __.GetInt64(i) = row.[int i] |> Unchecked.unbox
    override __.GetSByte(i) = row.[int i] |> Unchecked.unbox
    override __.GetUInt16(i) = row.[int i] |> Unchecked.unbox
    override __.GetUInt32(i) = row.[int i] |> Unchecked.unbox
    override __.GetUInt64(i) = row.[int i] |> Unchecked.unbox
    override __.GetSingle(i) = row.[int i] |> Unchecked.unbox
    override __.GetDouble(i) = row.[int i] |> Unchecked.unbox
    override __.GetDecimal(i) = row.[int i] |> Unchecked.unbox
    override __.GetDateTime(i) = row.[int i] |> Unchecked.unbox
    override __.GetBoolean(i) = row.[int i] |> Unchecked.unbox
    override __.GetGuid(i) = row.[int i] |> Unchecked.unbox

