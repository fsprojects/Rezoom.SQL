module Rezoom.SQL.Mapping.DataReader
open System
open System.Data

let private columnTypes =
    [|
        typeof<string>, ColumnType.String
        typeof<byte>, ColumnType.Byte
        typeof<int16>, ColumnType.Int16
        typeof<int32>, ColumnType.Int32
        typeof<int64>, ColumnType.Int64
        typeof<sbyte>, ColumnType.SByte
        typeof<uint16>, ColumnType.UInt16
        typeof<uint32>, ColumnType.UInt32
        typeof<uint64>, ColumnType.UInt64
        typeof<single>, ColumnType.Single
        typeof<double>, ColumnType.Double
        typeof<decimal>, ColumnType.Decimal
        typeof<DateTime>, ColumnType.DateTime
    |] |> dict

let columnType (ty : Type) =
  let succ, colTy = columnTypes.TryGetValue(ty)
  if succ then colTy else ColumnType.Object

let columnMap (reader : IDataReader) =
    let cols = Array.zeroCreate reader.FieldCount
    for i = 0 to reader.FieldCount - 1 do
        cols.[i] <- reader.GetName(i), columnType (reader.GetFieldType(i))
    ColumnMap.Parse(cols)

type DataReaderRow(reader : IDataReader) =
    inherit Row()
    override __.IsNull(i) = reader.IsDBNull(int i)
    override __.GetObject(i) = reader.GetValue(int i)
    override __.GetString(i) = reader.GetString(int i)
    override __.GetByte(i) = reader.GetByte(int i)
    override __.GetInt16(i) = reader.GetInt16(int i)
    override __.GetInt32(i) = reader.GetInt32(int i)
    override __.GetInt64(i) = reader.GetInt64(int i)
    override __.GetSByte(i) = reader.GetValue(int i) |> Convert.ToSByte
    override __.GetUInt16(i) = reader.GetValue(int i) |> Convert.ToUInt16
    override __.GetUInt32(i) = reader.GetValue(int i) |> Convert.ToUInt32
    override __.GetUInt64(i) = reader.GetValue(int i) |> Convert.ToUInt64
    override __.GetSingle(i) = reader.GetFloat(int i)
    override __.GetDouble(i) = reader.GetDouble(int i)
    override __.GetDecimal(i) = reader.GetDecimal(int i)
    override __.GetDateTime(i) = reader.GetDateTime(int i)

