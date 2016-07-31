namespace Rezoom.ORM
open System

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