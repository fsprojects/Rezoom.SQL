namespace Rezoom.ORM
open System

type RowValueType =
    | Object = 0 // whatever it is goes through boxing
    | String = 1
    | Byte = 2
    | Int16 = 3
    | Int32 = 4
    | Int64 = 5
    | Single = 6
    | Double = 7
    | Decimal = 8
    | DateTime = 9

[<AbstractClass>]
type Row() =
    abstract member Type : int -> RowValueType
    abstract member GetObject : int -> obj
    abstract member GetString : int -> string
    abstract member GetByte : int -> byte
    abstract member GetInt16 : int -> int16
    abstract member GetInt32 : int -> int32
    abstract member GetInt64 : int -> int64
    abstract member GetSingle : int -> single
    abstract member GetDouble : int -> double
    abstract member GetDecimal : int -> decimal
    abstract member GetDateTime : int -> DateTime