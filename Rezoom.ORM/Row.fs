namespace Rezoom.ORM
open System

type RowValueType =
    | Invalid  = 0s
    | Object   = 1s // whatever it is goes through boxing
    | String   = 2s
    | Byte     = 3s
    | Int16    = 4s
    | Int32    = 5s
    | Int64    = 6s
    | SByte    = 7s
    | UInt16   = 8s
    | UInt32   = 9s
    | UInt64   = 10s
    | Single   = 11s
    | Double   = 12s
    | Decimal  = 13s
    | DateTime = 14s

[<AbstractClass>]
type Row() =
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