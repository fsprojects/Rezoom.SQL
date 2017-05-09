module Rezoom.SQL.Mapping.CodeGeneration.PrimitiveConverters
open Rezoom.SQL.Mapping
open LicenseToCIL
open LicenseToCIL.Ops
open System
open System.Collections.Generic
open System.Globalization
open System.Reflection
open System.Reflection.Emit

let inline private convertVia convertFunction =
    fun o -> convertFunction(o, CultureInfo.InvariantCulture)

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
    | ColumnType.Object -> row.GetObject(col.Index) |> convertVia fromObj
    | ColumnType.String -> row.GetString(col.Index) |> fromString
    | ColumnType.Byte -> row.GetByte(col.Index) |> fromByte
    | ColumnType.Int16 -> row.GetInt16(col.Index) |> fromInt16
    | ColumnType.Int32 -> row.GetInt32(col.Index) |> fromInt32
    | ColumnType.Int64 -> row.GetInt64(col.Index) |> fromInt64
    | ColumnType.SByte -> row.GetSByte(col.Index) |> fromSByte
    | ColumnType.UInt16 -> row.GetUInt16(col.Index) |> fromUInt16
    | ColumnType.UInt32 -> row.GetUInt32(col.Index) |> fromUInt32
    | ColumnType.UInt64 -> row.GetUInt64(col.Index) |> fromUInt64
    | ColumnType.Single -> row.GetSingle(col.Index) |> fromSingle
    | ColumnType.Double -> row.GetDouble(col.Index) |> fromDouble
    | ColumnType.Decimal -> row.GetDecimal(col.Index) |> fromDecimal
    | x -> failwithf "Invalid column type %A for numeric" x

type Converters =
    static member ToObject(row : Row, col : ColumnInfo) = row.GetObject(col.Index)
    static member ToString(row : Row, col : ColumnInfo) =
        match col.Type with
        | ColumnType.String -> row.GetString(col.Index)
        | _ ->
            match row.GetObject(col.Index) with
            | null -> null
            | o -> Convert.ToString(o, CultureInfo.InvariantCulture)
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
    static member ToSingle(row : Row, col : ColumnInfo) : single =
        toNumeric row col
            Convert.ToSingle
            single single single single
            single single single single
            single single single single
    static member ToDouble(row : Row, col : ColumnInfo) : double =
        toNumeric row col
            Convert.ToDouble
            double double double double
            double double double double
            double double double double
    static member ToDecimal(row : Row, col : ColumnInfo) : decimal =
        toNumeric row col
            Convert.ToDecimal
            decimal decimal decimal decimal
            decimal decimal decimal decimal
            decimal decimal decimal decimal
    static member ToBoolean(row : Row, col : ColumnInfo) : bool =
        match col.Type with
        | ColumnType.Boolean -> row.GetBoolean(col.Index)
        | _ -> 0 <> Converters.ToInt32(row, col)
    static member ToDateTime(row : Row, col : ColumnInfo) : DateTime =
        let inline fromString str =
            DateTime.Parse(str, CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind)
        let dt =
            match col.Type with
            | ColumnType.DateTime -> row.GetDateTime(col.Index)
            | ColumnType.String -> row.GetString(col.Index) |> fromString
            | ColumnType.Object ->
                match row.GetObject(col.Index) with
                | :? string as s -> fromString s
                | o -> Convert.ToDateTime(o, CultureInfo.InvariantCulture)
            | x -> failwithf "Invalid column type %A for DateTime" x
        if dt.Kind = DateTimeKind.Unspecified then
            DateTime.SpecifyKind(dt, DateTimeKind.Utc)
        else
            dt.ToUniversalTime()

    static member ToDateTimeOffset(row : Row, col : ColumnInfo) : DateTimeOffset =
        let inline fromString str =
            DateTimeOffset.Parse(str, CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind)
        let inline fromDateTime (dt : DateTime) =
            // assume if we're putting datetimes in the DB, they are UTC unless specified local
            if dt.Kind = DateTimeKind.Unspecified then
                DateTimeOffset(dt, TimeSpan.Zero)
            else
                DateTimeOffset(dt)
        match col.Type with
        | ColumnType.DateTimeOffset -> row.GetObject(col.Index) |> Unchecked.unbox
        | ColumnType.String -> row.GetString(col.Index) |> fromString
        | ColumnType.Object ->
            match row.GetObject(col.Index) with
            | :? string as s -> fromString s
            | :? DateTime as dt -> fromDateTime dt
            | o -> Unchecked.unbox o
        | ColumnType.DateTime ->
            let dt = row.GetDateTime(col.Index)
            fromDateTime dt
        | x -> failwithf "Invalid column type %A for DateTimeOffset" x

    static member ToGuid(row : Row, col : ColumnInfo) : Guid =
        let inline fromString str = Guid.Parse(str)
        let inline fromBytes bytes = Guid(bytes : byte array)
        match col.Type with
        | ColumnType.Guid -> row.GetGuid(col.Index)
        | ColumnType.String -> row.GetString(col.Index) |> fromString
        | ColumnType.Object ->
            match row.GetObject(col.Index) with
            | :? string as s -> fromString s
            | :? array<byte> as bytes -> fromBytes bytes
            | o -> Unchecked.unbox o
        | x -> failwithf "Invalid column type %A for Guid" x


let private convertersByType =
    let methods = typeof<Converters>.GetMethods()
    methods
    |> Seq.filter
        (fun m ->
            let parTypes = m.GetParameters() |> Array.map (fun p -> p.ParameterType)
            parTypes = [|typeof<Row>; typeof<ColumnInfo>|])
    |> Seq.map
        (fun m -> m.ReturnType, m)
    |> dict

let private columnIndexField = typeof<ColumnInfo>.GetField("Index")
let private columnTypeField = typeof<ColumnInfo>.GetField("Type")
let private rowIsNullMethod = typeof<Row>.GetMethod("IsNull")
let private rowGetStringMethod = typeof<Row>.GetMethod("GetString")
let private stringTrimMethod = typeof<string>.GetMethod("Trim", Type.EmptyTypes)

let private storeInstructions=
    [
        typeof<byte>, stind'i1
        typeof<sbyte>, stind'i1
        typeof<uint16>, stind'i2
        typeof<int16>, stind'i2
        typeof<uint32>, stind'i4
        typeof<int32>, stind'i4
        typeof<uint64>, stind'i8
        typeof<int64>, stind'i8
    ] |> dict

let private enumTryParser (delTy) (enumTy : Type) =
    let underlying = enumTy.GetEnumUnderlyingType()
    let loadValue =
        if obj.ReferenceEquals(underlying, typeof<int64>) then fun o -> ldc'i8 (Unchecked.unbox o)
        elif obj.ReferenceEquals(underlying, typeof<uint64>) then fun o -> ldc'i8 (int64 (Unchecked.unbox o : uint64))
        elif obj.ReferenceEquals(underlying, typeof<uint32>) then fun o -> ldc'i4 (int (Unchecked.unbox o : uint32))
        else fun (o : obj) -> ldc'i4 (Convert.ToInt32(o))
    let storeValue = storeInstructions.[underlying]
    let names = Enum.GetNames(enumTy)
    let values = Enum.GetValues(enumTy)
    let pairs =
        seq {
            for i = 0 to names.Length - 1 do
                yield names.[i], values.GetValue(i)
        }
    let dynamicMethod = 
        DynamicMethod
            ( "TryParse" + enumTy.Name
            , typeof<bool>
            , [| typeof<string>; enumTy.MakeByRefType() |]
            , typeof<Converters>
            )
    (cil {
        yield ldarg 0
        yield call1 stringTrimMethod
        yield StringSwitch.insensitive
            [| for name, value in pairs ->
                name,
                    cil {
                        yield ldarg 1
                        yield loadValue value
                        yield storeValue
                        yield ldc'i4 1
                        yield ret
                    }
            |] zero
        yield ldc'i4 0
        yield ret
    }) null (IL(dynamicMethod.GetILGenerator())) |> ignore
    dynamicMethod.CreateDelegate(delTy)

type EnumTryParserDelegate<'enum> = delegate of string * 'enum byref -> bool

type EnumTryParser<'enum>() =
    static let parser =
        enumTryParser typeof<EnumTryParserDelegate<'enum>> typeof<'enum>
        |> Unchecked.unbox : EnumTryParserDelegate<'enum>
    static member TryParse(str : string, enum : 'enum byref) =
        parser.Invoke(str, &enum)

let rec converter (ty : Type) : RowConversionMethod option =
    let succ, meth = convertersByType.TryGetValue(ty)
    if succ then
        Some (Ops.call2 meth)
    elif ty.IsEnum then
        match converter (ty.GetEnumUnderlyingType()) with
        | None -> None
        | Some converter ->
            cil {
                let! colInfo = tmplocal typeof<ColumnInfo>
                let! parsed = tmplocal ty
                let! skipParse = deflabel
                let! exit = deflabel
                yield dup // row, col, col
                yield stloc colInfo // row, col
                yield ldfld columnTypeField // row, type
                yield ldc'i4 (int ColumnType.String) // row, type, string
                yield bne'un's skipParse // row
                yield dup // row, row
                yield ldloc colInfo // row, row, col
                yield ldfld columnIndexField // row, row, i
                yield callvirt2 rowGetStringMethod // row, string
                yield ldloca parsed // row, string, &parsed
                yield call2 <| typedefof<_ EnumTryParser>.MakeGenericType(ty).GetMethod("TryParse") // row, succ
                yield brfalse's skipParse // row
                yield pop
                yield ldloc parsed
                yield br's exit
                yield mark skipParse
                yield ldloc colInfo
                yield converter
                yield mark exit
            } |> Some
    else genericConverter ty

and genericConverter (ty : Type) : RowConversionMethod option =
    if ty.IsConstructedGenericType then
        let def = ty.GetGenericTypeDefinition()
        if def = typedefof<_ Nullable> then
            match ty.GetGenericArguments() with
            | [| nTy |] ->
                match converter nTy with
                | None -> None
                | Some innerConverter ->
                cil {
                    let! colInfo = tmplocal typeof<ColumnInfo>
                    let! ncase = deflabel
                    let! exit = deflabel
                    yield stloc colInfo // row
                    yield dup // row, row
                    yield ldloc colInfo // row, row, col
                    yield ldfld columnIndexField // row, row, index
                    yield Ops.callvirt2 rowIsNullMethod // row, isnull
                    yield brtrue's ncase
                    yield cil {
                        yield ldloc colInfo
                        yield innerConverter
                        yield newobj1 (ty.GetConstructor([| nTy |]))
                        yield br's exit
                    }
                    yield mark ncase
                    yield cil {
                        yield pop
                        let! empty = tmplocal ty
                        yield ldloca empty
                        yield initobj ty
                        yield ldloc empty
                    }
                    yield mark exit
                } |> Some
            | _ -> failwith "Cannot function in world where Nullable<T> doesn't have one type argument."
        elif def = typedefof<_ option> then
            match ty.GetGenericArguments() with
            | [| nTy |] ->
                match converter nTy with
                | None -> None
                | Some innerConverter ->
                cil {
                    let! colInfo = tmplocal typeof<ColumnInfo>
                    let! ncase = deflabel
                    let! exit = deflabel
                    yield stloc colInfo // row
                    yield dup // row, row
                    yield ldloc colInfo // row, row, col
                    yield ldfld columnIndexField // row, row, index
                    yield Ops.callvirt2 rowIsNullMethod // row, isnull
                    yield brtrue's ncase
                    yield cil {
                        yield ldloc colInfo
                        yield innerConverter
                        yield newobj1 (ty.GetConstructor([| nTy |]))
                        yield br's exit
                    }
                    yield mark ncase
                    yield cil {
                        yield pop
                        yield ldnull // None
                    }
                    yield mark exit
                } |> Some
            | _ -> failwith "Cannot function in world where FSharpOption<T> doesn't have one type argument."
        else None
    else None