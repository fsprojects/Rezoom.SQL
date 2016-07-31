module private Rezoom.ORM.PrimitiveConverters
open LicenseToCIL
open System
open System.Collections.Generic

let inline private toNumeric
    (row : Row)
    (col : ColumnInfo)
    fromObj
    fromString
    fromByte
    fromInt16
    fromInt32
    fromInt64
    fromSByte
    fromUInt16
    fromUInt32
    fromUInt64
    fromSingle
    fromDouble
    fromDecimal =
    match col.Type with
    | RowValueType.Object -> row.GetObject(col.Index) |> fromObj
    | RowValueType.String -> row.GetString(col.Index) |> fromString
    | RowValueType.Byte -> row.GetByte(col.Index) |> fromByte
    | RowValueType.Int16 -> row.GetInt16(col.Index) |> fromInt16
    | RowValueType.Int32 -> row.GetInt32(col.Index) |> fromInt32
    | RowValueType.Int64 -> row.GetInt16(col.Index) |> fromInt64
    | RowValueType.SByte -> row.GetSByte(col.Index) |> fromSByte
    | RowValueType.UInt16 -> row.GetUInt16(col.Index) |> fromUInt16
    | RowValueType.UInt32 -> row.GetUInt32(col.Index) |> fromUInt32
    | RowValueType.UInt64 -> row.GetUInt64(col.Index) |> fromUInt64
    | RowValueType.Single -> row.GetSingle(col.Index) |> fromSingle
    | RowValueType.Double -> row.GetDouble(col.Index) |> fromDouble
    | RowValueType.Decimal -> row.GetDecimal(col.Index) |> fromDecimal
    | RowValueType.DateTime -> failwith "Invalid column type DateTime for numeric"
    | x -> failwithf "Invalid column type %A for numeric" x

type Converter =
    static member ToByteArray(row : Row, col : ColumnInfo) =
        row.GetObject(col.Index)
        |> Unchecked.unbox : byte array
    static member ToByte(row : Row, col : ColumnInfo) : byte =
        toNumeric row col
            Convert.ToByte
            byte byte byte byte
            byte byte byte byte
            byte byte byte byte
    static member ToInt16(row : Row, col : ColumnInfo) : int16 =
        toNumeric row col
            Convert.ToInt16
            int16 int16 int16 int16
            int16 int16 int16 int16
            int16 int16 int16 int16
    static member ToInt32(row : Row, col : ColumnInfo) : int32 =
        toNumeric row col
            Convert.ToInt32
            int32 int32 int32 int32
            int32 int32 int32 int32
            int32 int32 int32 int32
    static member ToInt64(row : Row, col : ColumnInfo) : int64 =
        toNumeric row col
            Convert.ToInt64
            int64 int64 int64 int64
            int64 int64 int64 int64
            int64 int64 int64 int64
    static member ToSByte(row : Row, col : ColumnInfo) : sbyte =
        toNumeric row col
            Convert.ToSByte
            sbyte sbyte sbyte sbyte
            sbyte sbyte sbyte sbyte
            sbyte sbyte sbyte sbyte
    static member ToUInt16(row : Row, col : ColumnInfo) : uint16 =
        toNumeric row col
            Convert.ToUInt16
            uint16 uint16 uint16 uint16
            uint16 uint16 uint16 uint16
            uint16 uint16 uint16 uint16
    static member ToUInt32(row : Row, col : ColumnInfo) : uint32 =
        toNumeric row col
            Convert.ToUInt32
            uint32 uint32 uint32 uint32
            uint32 uint32 uint32 uint32
            uint32 uint32 uint32 uint32
    static member ToUInt64(row : Row, col : ColumnInfo) : uint64 =
        toNumeric row col
            Convert.ToUInt64
            uint64 uint64 uint64 uint64
            uint64 uint64 uint64 uint64
            uint64 uint64 uint64 uint64