using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Reflection;

namespace Rezoom.ADO.Materialization
{
    public static class PrimitiveConverter
    {
        public static string ToString(object obj) => obj?.ToString();

        private enum JumpTag
        {
            Int32,
            Int64,
            Int16,
            Byte,
            Double,
            Float,
            Decimal,
            String,
            UInt32,
            UInt64,
            UInt16,
            SByte,
            Char,
            Invalid,
        }

        #region Signed Integers

        #region FastInt8
        private static JumpTag _t8;
        public static sbyte ToInt8(object obj)
        {
            var thru0 = false;
            switch (_t8)
            {
                case JumpTag.Int32:
                    thru0 = true;
                    if (obj is int) { _t8 = JumpTag.Int32; return (sbyte)(int)obj; }
                    goto case JumpTag.Int64;
                case JumpTag.Int64:
                    if (obj is long) { _t8 = JumpTag.Int64; return (sbyte)(long)obj; }
                    goto case JumpTag.Int16;
                case JumpTag.Int16:
                    if (obj is short) { _t8 = JumpTag.Int16; return (sbyte)(short)obj; }
                    goto case JumpTag.Byte;
                case JumpTag.Byte:
                    if (obj is byte) { _t8 = JumpTag.Byte; return (sbyte)(byte)obj; }
                    goto case JumpTag.Double;
                case JumpTag.Double:
                    if (obj is double) { _t8 = JumpTag.Double; return (sbyte)(double)obj; }
                    goto case JumpTag.Float;
                case JumpTag.Float:
                    if (obj is float) { _t8 = JumpTag.Float; return (sbyte)(float)obj; }
                    goto case JumpTag.Decimal;
                case JumpTag.Decimal:
                    if (obj is decimal) { _t8 = JumpTag.Decimal; return (sbyte)(decimal)obj; }
                    goto case JumpTag.String;
                case JumpTag.String:
                    var str = obj as string;
                    if (str != null) { _t8 = JumpTag.String; return sbyte.Parse(str, CultureInfo.InvariantCulture); }
                    goto case JumpTag.UInt32;
                case JumpTag.UInt32:
                    if (obj is uint) { _t8 = JumpTag.UInt32; return (sbyte)(uint)obj; }
                    goto case JumpTag.UInt64;
                case JumpTag.UInt64:
                    if (obj is ulong) { _t8 = JumpTag.UInt64; return (sbyte)(ulong)obj; }
                    goto case JumpTag.UInt16;
                case JumpTag.UInt16:
                    if (obj is ushort) { _t8 = JumpTag.UInt16; return (sbyte)(ushort)obj; }
                    goto case JumpTag.SByte;
                case JumpTag.SByte:
                    if (obj is sbyte) { _t8 = JumpTag.SByte; return (sbyte)obj; }
                    goto case JumpTag.Char;
                case JumpTag.Char:
                    if (obj is char) { _t8 = JumpTag.Char; return (sbyte)(char)obj; }
                    goto case JumpTag.Invalid;
                case JumpTag.Invalid:
                    if (thru0) goto default;
                    goto case 0;
                default:
                    throw new ArgumentOutOfRangeException(nameof(obj), $"Can't convert {obj} to SByte");
            }
        }
        #endregion

        public static sbyte? ToNullableInt8(object obj) => obj == null ? (sbyte?)null : ToInt8(obj);

        #region FastInt16
        private static JumpTag _t16;
        public static short ToInt16(object obj)
        {
            var thru0 = false;
            switch (_t16)
            {
                case JumpTag.Int32:
                    thru0 = true;
                    if (obj is int) { _t16 = JumpTag.Int32; return (short)(int)obj; }
                    goto case JumpTag.Int64;
                case JumpTag.Int64:
                    if (obj is long) { _t16 = JumpTag.Int64; return (short)(long)obj; }
                    goto case JumpTag.Int16;
                case JumpTag.Int16:
                    if (obj is short) { _t16 = JumpTag.Int16; return (short)obj; }
                    goto case JumpTag.Byte;
                case JumpTag.Byte:
                    if (obj is byte) { _t16 = JumpTag.Byte; return (byte)obj; }
                    goto case JumpTag.Double;
                case JumpTag.Double:
                    if (obj is double) { _t16 = JumpTag.Double; return (short)(double)obj; }
                    goto case JumpTag.Float;
                case JumpTag.Float:
                    if (obj is float) { _t16 = JumpTag.Float; return (short)(float)obj; }
                    goto case JumpTag.Decimal;
                case JumpTag.Decimal:
                    if (obj is decimal) { _t16 = JumpTag.Decimal; return (short)(decimal)obj; }
                    goto case JumpTag.String;
                case JumpTag.String:
                    var str = obj as string;
                    if (str != null) { _t16 = JumpTag.String; return short.Parse(str, CultureInfo.InvariantCulture); }
                    goto case JumpTag.UInt32;
                case JumpTag.UInt32:
                    if (obj is uint) { _t16 = JumpTag.UInt32; return (short)(uint)obj; }
                    goto case JumpTag.UInt64;
                case JumpTag.UInt64:
                    if (obj is ulong) { _t16 = JumpTag.UInt64; return (short)(ulong)obj; }
                    goto case JumpTag.UInt16;
                case JumpTag.UInt16:
                    if (obj is ushort) { _t16 = JumpTag.UInt16; return (short)(ushort)obj; }
                    goto case JumpTag.SByte;
                case JumpTag.SByte:
                    if (obj is sbyte) { _t16 = JumpTag.SByte; return (sbyte)obj; }
                    goto case JumpTag.Char;
                case JumpTag.Char:
                    if (obj is char) { _t16 = JumpTag.Char; return (short)(char)obj; }
                    goto case JumpTag.Invalid;
                case JumpTag.Invalid:
                    if (thru0) goto default;
                    goto case 0;
                default:
                    throw new ArgumentOutOfRangeException(nameof(obj), $"Can't convert {obj} to Int16");
            }
        }
        #endregion

        public static short? ToNullableInt16(object obj) => obj == null ? (short?)null : ToInt16(obj);

        #region FastInt32
        private static JumpTag _t32;
        public static int ToInt32(object obj)
        {
            var thru0 = false;
            switch (_t32)
            {
                case JumpTag.Int32:
                    thru0 = true;
                    if (obj is int) { _t32 = JumpTag.Int32; return (int)obj; }
                    goto case JumpTag.Int64;
                case JumpTag.Int64:
                    if (obj is long) { _t32 = JumpTag.Int64; return (int)(long)obj; }
                    goto case JumpTag.Int16;
                case JumpTag.Int16:
                    if (obj is short) { _t32 = JumpTag.Int16; return (short)obj; }
                    goto case JumpTag.Byte;
                case JumpTag.Byte:
                    if (obj is byte) { _t32 = JumpTag.Byte; return (byte)obj; }
                    goto case JumpTag.Double;
                case JumpTag.Double:
                    if (obj is double) { _t32 = JumpTag.Double; return (int)(double)obj; }
                    goto case JumpTag.Float;
                case JumpTag.Float:
                    if (obj is float) { _t32 = JumpTag.Float; return (int)(float)obj; }
                    goto case JumpTag.Decimal;
                case JumpTag.Decimal:
                    if (obj is decimal) { _t32 = JumpTag.Decimal; return (int)(decimal)obj; }
                    goto case JumpTag.String;
                case JumpTag.String:
                    var str = obj as string;
                    if (str != null) { _t32 = JumpTag.String; return int.Parse(str, CultureInfo.InvariantCulture); }
                    goto case JumpTag.UInt32;
                case JumpTag.UInt32:
                    if (obj is uint) { _t32 = JumpTag.UInt32; return (int)(uint)obj; }
                    goto case JumpTag.UInt64;
                case JumpTag.UInt64:
                    if (obj is ulong) { _t32 = JumpTag.UInt64; return (int)(ulong)obj; }
                    goto case JumpTag.UInt16;
                case JumpTag.UInt16:
                    if (obj is ushort) { _t32 = JumpTag.UInt16; return (ushort)obj; }
                    goto case JumpTag.SByte;
                case JumpTag.SByte:
                    if (obj is sbyte) { _t32 = JumpTag.SByte; return (sbyte)obj; }
                    goto case JumpTag.Char;
                case JumpTag.Char:
                    if (obj is char) { _t32 = JumpTag.Char; return (char)obj; }
                    goto case JumpTag.Invalid;
                case JumpTag.Invalid:
                    if (thru0) goto default;
                    goto case 0;
                default:
                    throw new ArgumentOutOfRangeException(nameof(obj), $"Can't convert {obj} to Int32");
            }
        }
        #endregion FastInt32

        public static int? ToNullableInt32(object obj) => obj == null ? (int?)null : ToInt32(obj);

        #region FastInt64
        private static JumpTag _t64;
        public static long ToInt64(object obj)
        {
            var thru0 = false;
            switch (_t64)
            {
                case JumpTag.Int32:
                    thru0 = true;
                    if (obj is int) { _t64 = JumpTag.Int32; return (int)obj; }
                    goto case JumpTag.Int64;
                case JumpTag.Int64:
                    if (obj is long) { _t64 = JumpTag.Int64; return (long)obj; }
                    goto case JumpTag.Int16;
                case JumpTag.Int16:
                    if (obj is short) { _t64 = JumpTag.Int16; return (short)obj; }
                    goto case JumpTag.Byte;
                case JumpTag.Byte:
                    if (obj is byte) { _t64 = JumpTag.Byte; return (byte)obj; }
                    goto case JumpTag.Double;
                case JumpTag.Double:
                    if (obj is double) { _t64 = JumpTag.Double; return (long)(double)obj; }
                    goto case JumpTag.Float;
                case JumpTag.Float:
                    if (obj is float) { _t64 = JumpTag.Float; return (long)(float)obj; }
                    goto case JumpTag.Decimal;
                case JumpTag.Decimal:
                    if (obj is decimal) { _t64 = JumpTag.Decimal; return (long)(decimal)obj; }
                    goto case JumpTag.String;
                case JumpTag.String:
                    var str = obj as string;
                    if (str != null) { _t64 = JumpTag.String; return long.Parse(str, CultureInfo.InvariantCulture); }
                    goto case JumpTag.UInt32;
                case JumpTag.UInt32:
                    if (obj is uint) { _t64 = JumpTag.UInt32; return (uint)obj; }
                    goto case JumpTag.UInt64;
                case JumpTag.UInt64:
                    if (obj is ulong) { _t64 = JumpTag.UInt64; return (long)(ulong)obj; }
                    goto case JumpTag.UInt16;
                case JumpTag.UInt16:
                    if (obj is ushort) { _t64 = JumpTag.UInt16; return (ushort)obj; }
                    goto case JumpTag.SByte;
                case JumpTag.SByte:
                    if (obj is sbyte) { _t64 = JumpTag.SByte; return (sbyte)obj; }
                    goto case JumpTag.Char;
                case JumpTag.Char:
                    if (obj is char) { _t64 = JumpTag.Char; return (char)obj; }
                    goto case JumpTag.Invalid;
                case JumpTag.Invalid:
                    if (thru0) goto default;
                    goto case 0;
                default:
                    throw new ArgumentOutOfRangeException(nameof(obj), $"Can't convert {obj} to Int64");
            }
        }
        #endregion

        public static long? ToNullableInt64(object obj) => obj == null ? (long?)null : ToInt64(obj);

        #endregion

        #region Unsigned Integers

        #region FastUInt8
        private static JumpTag _tu8;
        public static byte ToUInt8(object obj)
        {
            var thru0 = false;
            switch (_tu8)
            {
                case JumpTag.Int32:
                    thru0 = true;
                    if (obj is int) { _tu8 = JumpTag.Int32; return (byte)(int)obj; }
                    goto case JumpTag.Int64;
                case JumpTag.Int64:
                    if (obj is long) { _tu8 = JumpTag.Int64; return (byte)(long)obj; }
                    goto case JumpTag.Int16;
                case JumpTag.Int16:
                    if (obj is short) { _tu8 = JumpTag.Int16; return (byte)(short)obj; }
                    goto case JumpTag.Byte;
                case JumpTag.Byte:
                    if (obj is byte) { _tu8 = JumpTag.Byte; return (byte)obj; }
                    goto case JumpTag.Double;
                case JumpTag.Double:
                    if (obj is double) { _tu8 = JumpTag.Double; return (byte)(double)obj; }
                    goto case JumpTag.Float;
                case JumpTag.Float:
                    if (obj is float) { _tu8 = JumpTag.Float; return (byte)(float)obj; }
                    goto case JumpTag.Decimal;
                case JumpTag.Decimal:
                    if (obj is decimal) { _tu8 = JumpTag.Decimal; return (byte)(decimal)obj; }
                    goto case JumpTag.String;
                case JumpTag.String:
                    var str = obj as string;
                    if (str != null) { _tu8 = JumpTag.String; return byte.Parse(str, CultureInfo.InvariantCulture); }
                    goto case JumpTag.UInt32;
                case JumpTag.UInt32:
                    if (obj is uint) { _tu8 = JumpTag.UInt32; return (byte)(uint)obj; }
                    goto case JumpTag.UInt64;
                case JumpTag.UInt64:
                    if (obj is ulong) { _tu8 = JumpTag.UInt64; return (byte)(ulong)obj; }
                    goto case JumpTag.UInt16;
                case JumpTag.UInt16:
                    if (obj is ushort) { _tu8 = JumpTag.UInt16; return (byte)(ushort)obj; }
                    goto case JumpTag.SByte;
                case JumpTag.SByte:
                    if (obj is sbyte) { _tu8 = JumpTag.SByte; return (byte)(sbyte)obj; }
                    goto case JumpTag.Char;
                case JumpTag.Char:
                    if (obj is char) { _tu8 = JumpTag.Char; return (byte)(char)obj; }
                    goto case JumpTag.Invalid;
                case JumpTag.Invalid:
                    if (thru0) goto default;
                    goto case 0;
                default:
                    throw new ArgumentOutOfRangeException(nameof(obj), $"Can't convert {obj} to Byte");
            }
        }
        #endregion

        public static byte? ToNullableUInt8(object obj) => obj == null ? (byte?)null : ToUInt8(obj);

        #region FastUInt16
        private static JumpTag _tu16;
        public static ushort ToUInt16(object obj)
        {
            var thru0 = false;
            switch (_tu16)
            {
                case JumpTag.Int32:
                    thru0 = true;
                    if (obj is int) { _tu16 = JumpTag.Int32; return (ushort)(int)obj; }
                    goto case JumpTag.Int64;
                case JumpTag.Int64:
                    if (obj is long) { _tu16 = JumpTag.Int64; return (ushort)(long)obj; }
                    goto case JumpTag.Int16;
                case JumpTag.Int16:
                    if (obj is short) { _tu16 = JumpTag.Int16; return (ushort)(short)obj; }
                    goto case JumpTag.Byte;
                case JumpTag.Byte:
                    if (obj is byte) { _tu16 = JumpTag.Byte; return (byte)obj; }
                    goto case JumpTag.Double;
                case JumpTag.Double:
                    if (obj is double) { _tu16 = JumpTag.Double; return (ushort)(double)obj; }
                    goto case JumpTag.Float;
                case JumpTag.Float:
                    if (obj is float) { _tu16 = JumpTag.Float; return (ushort)(float)obj; }
                    goto case JumpTag.Decimal;
                case JumpTag.Decimal:
                    if (obj is decimal) { _tu16 = JumpTag.Decimal; return (ushort)(decimal)obj; }
                    goto case JumpTag.String;
                case JumpTag.String:
                    var str = obj as string;
                    if (str != null) { _tu16 = JumpTag.String; return ushort.Parse(str, CultureInfo.InvariantCulture); }
                    goto case JumpTag.UInt32;
                case JumpTag.UInt32:
                    if (obj is uint) { _tu16 = JumpTag.UInt32; return (ushort)(uint)obj; }
                    goto case JumpTag.UInt64;
                case JumpTag.UInt64:
                    if (obj is ulong) { _tu16 = JumpTag.UInt64; return (ushort)(ulong)obj; }
                    goto case JumpTag.UInt16;
                case JumpTag.UInt16:
                    if (obj is ushort) { _tu16 = JumpTag.UInt16; return (ushort)obj; }
                    goto case JumpTag.SByte;
                case JumpTag.SByte:
                    if (obj is sbyte) { _tu16 = JumpTag.SByte; return (ushort)(sbyte)obj; }
                    goto case JumpTag.Char;
                case JumpTag.Char:
                    if (obj is char) { _tu16 = JumpTag.Char; return (char)obj; }
                    goto case JumpTag.Invalid;
                case JumpTag.Invalid:
                    if (thru0) goto default;
                    goto case 0;
                default:
                    throw new ArgumentOutOfRangeException(nameof(obj), $"Can't convert {obj} to UInt16");
            }
        }
        #endregion

        public static ushort? ToNullableUInt16(object obj) => obj == null ? (ushort?)null : ToUInt16(obj);

        #region FastUInt32
        private static JumpTag _tu32;
        public static uint ToUInt32(object obj)
        {
            var thru0 = false;
            switch (_tu32)
            {
                case JumpTag.Int32:
                    thru0 = true;
                    if (obj is int) { _tu32 = JumpTag.Int32; return (uint)(int)obj; }
                    goto case JumpTag.Int64;
                case JumpTag.Int64:
                    if (obj is long) { _tu32 = JumpTag.Int64; return (uint)(long)obj; }
                    goto case JumpTag.Int16;
                case JumpTag.Int16:
                    if (obj is short) { _tu32 = JumpTag.Int16; return (uint)(short)obj; }
                    goto case JumpTag.Byte;
                case JumpTag.Byte:
                    if (obj is byte) { _tu32 = JumpTag.Byte; return (byte)obj; }
                    goto case JumpTag.Double;
                case JumpTag.Double:
                    if (obj is double) { _tu32 = JumpTag.Double; return (uint)(double)obj; }
                    goto case JumpTag.Float;
                case JumpTag.Float:
                    if (obj is float) { _tu32 = JumpTag.Float; return (uint)(float)obj; }
                    goto case JumpTag.Decimal;
                case JumpTag.Decimal:
                    if (obj is decimal) { _tu32 = JumpTag.Decimal; return (uint)(decimal)obj; }
                    goto case JumpTag.String;
                case JumpTag.String:
                    var str = obj as string;
                    if (str != null) { _tu32 = JumpTag.String; return uint.Parse(str, CultureInfo.InvariantCulture); }
                    goto case JumpTag.UInt32;
                case JumpTag.UInt32:
                    if (obj is uint) { _tu32 = JumpTag.UInt32; return (uint)obj; }
                    goto case JumpTag.UInt64;
                case JumpTag.UInt64:
                    if (obj is ulong) { _tu32 = JumpTag.UInt64; return (uint)(ulong)obj; }
                    goto case JumpTag.UInt16;
                case JumpTag.UInt16:
                    if (obj is ushort) { _tu32 = JumpTag.UInt16; return (ushort)obj; }
                    goto case JumpTag.SByte;
                case JumpTag.SByte:
                    if (obj is sbyte) { _tu32 = JumpTag.SByte; return (uint)(sbyte)obj; }
                    goto case JumpTag.Char;
                case JumpTag.Char:
                    if (obj is char) { _tu32 = JumpTag.Char; return (char)obj; }
                    goto case JumpTag.Invalid;
                case JumpTag.Invalid:
                    if (thru0) goto default;
                    goto case 0;
                default:
                    throw new ArgumentOutOfRangeException(nameof(obj), $"Can't convert {obj} to UInt32");
            }
        }
        #endregion

        public static uint? ToNullableUInt32(object obj) => obj == null ? (uint?)null : ToUInt32(obj);

        #region FastUInt64
        private static JumpTag _tu64;
        public static ulong ToUInt64(object obj)
        {
            var thru0 = false;
            switch (_tu64)
            {
                case JumpTag.Int32:
                    thru0 = true;
                    if (obj is int) { _tu64 = JumpTag.Int32; return (ulong)(int)obj; }
                    goto case JumpTag.Int64;
                case JumpTag.Int64:
                    if (obj is long) { _tu64 = JumpTag.Int64; return (ulong)(long)obj; }
                    goto case JumpTag.Int16;
                case JumpTag.Int16:
                    if (obj is short) { _tu64 = JumpTag.Int16; return (ulong)(short)obj; }
                    goto case JumpTag.Byte;
                case JumpTag.Byte:
                    if (obj is byte) { _tu64 = JumpTag.Byte; return (byte)obj; }
                    goto case JumpTag.Double;
                case JumpTag.Double:
                    if (obj is double) { _tu64 = JumpTag.Double; return (ulong)(double)obj; }
                    goto case JumpTag.Float;
                case JumpTag.Float:
                    if (obj is float) { _tu64 = JumpTag.Float; return (ulong)(float)obj; }
                    goto case JumpTag.Decimal;
                case JumpTag.Decimal:
                    if (obj is decimal) { _tu64 = JumpTag.Decimal; return (ulong)(decimal)obj; }
                    goto case JumpTag.String;
                case JumpTag.String:
                    var str = obj as string;
                    if (str != null) { _tu64 = JumpTag.String; return ulong.Parse(str, CultureInfo.InvariantCulture); }
                    goto case JumpTag.UInt32;
                case JumpTag.UInt32:
                    if (obj is uint) { _tu64 = JumpTag.UInt32; return (uint)obj; }
                    goto case JumpTag.UInt64;
                case JumpTag.UInt64:
                    if (obj is ulong) { _tu64 = JumpTag.UInt64; return (ulong)obj; }
                    goto case JumpTag.UInt16;
                case JumpTag.UInt16:
                    if (obj is ushort) { _tu64 = JumpTag.UInt16; return (ushort)obj; }
                    goto case JumpTag.SByte;
                case JumpTag.SByte:
                    if (obj is sbyte) { _tu64 = JumpTag.SByte; return (ulong)(sbyte)obj; }
                    goto case JumpTag.Char;
                case JumpTag.Char:
                    if (obj is char) { _tu64 = JumpTag.Char; return (char)obj; }
                    goto case JumpTag.Invalid;
                case JumpTag.Invalid:
                    if (thru0) goto default;
                    goto case 0;
                default:
                    throw new ArgumentOutOfRangeException(nameof(obj), $"Can't convert {obj} to UInt64");
            }
        }
        #endregion

        public static ulong? ToNullableUInt64(object obj) => obj == null ? (ulong?)null : ToUInt64(obj);

        #endregion

        #region Floating Point

        #region FastSingle
        private static JumpTag _tf32;
        public static float ToSingle(object obj)
        {
            var thru0 = false;
            switch (_tf32)
            {
                case JumpTag.Int32:
                    thru0 = true;
                    if (obj is int) { _tf32 = JumpTag.Int32; return (int)obj; }
                    goto case JumpTag.Int64;
                case JumpTag.Int64:
                    if (obj is long) { _tf32 = JumpTag.Int64; return (long)obj; }
                    goto case JumpTag.Int16;
                case JumpTag.Int16:
                    if (obj is short) { _tf32 = JumpTag.Int16; return (short)obj; }
                    goto case JumpTag.Byte;
                case JumpTag.Byte:
                    if (obj is byte) { _tf32 = JumpTag.Byte; return (byte)obj; }
                    goto case JumpTag.Double;
                case JumpTag.Double:
                    if (obj is double) { _tf32 = JumpTag.Double; return (float)(double)obj; }
                    goto case JumpTag.Float;
                case JumpTag.Float:
                    if (obj is float) { _tf32 = JumpTag.Float; return (float)obj; }
                    goto case JumpTag.Decimal;
                case JumpTag.Decimal:
                    if (obj is decimal) { _tf32 = JumpTag.Decimal; return (float)(decimal)obj; }
                    goto case JumpTag.String;
                case JumpTag.String:
                    var str = obj as string;
                    if (str != null) { _tf32 = JumpTag.String; return float.Parse(str, CultureInfo.InvariantCulture); }
                    goto case JumpTag.UInt32;
                case JumpTag.UInt32:
                    if (obj is uint) { _tf32 = JumpTag.UInt32; return (uint)obj; }
                    goto case JumpTag.UInt64;
                case JumpTag.UInt64:
                    if (obj is ulong) { _tf32 = JumpTag.UInt64; return (ulong)obj; }
                    goto case JumpTag.UInt16;
                case JumpTag.UInt16:
                    if (obj is ushort) { _tf32 = JumpTag.UInt16; return (ushort)obj; }
                    goto case JumpTag.SByte;
                case JumpTag.SByte:
                    if (obj is sbyte) { _tf32 = JumpTag.SByte; return (sbyte)obj; }
                    goto case JumpTag.Char;
                case JumpTag.Char:
                    if (obj is char) { _tf32 = JumpTag.Char; return (char)obj; }
                    goto case JumpTag.Invalid;
                case JumpTag.Invalid:
                    if (thru0) goto default;
                    goto case 0;
                default:
                    throw new ArgumentOutOfRangeException(nameof(obj), $"Can't convert {obj} to Single");
            }
        }
        #endregion

        public static float? ToNullableSingle(object obj) => obj == null ? (float?)null : ToSingle(obj);

        #region FastDouble
        private static JumpTag _tf64;
        public static double ToDouble(object obj)
        {
            var thru0 = false;
            switch (_tf64)
            {
                case JumpTag.Int32:
                    thru0 = true;
                    if (obj is int) { _tf64 = JumpTag.Int32; return (int)obj; }
                    goto case JumpTag.Int64;
                case JumpTag.Int64:
                    if (obj is long) { _tf64 = JumpTag.Int64; return (long)obj; }
                    goto case JumpTag.Int16;
                case JumpTag.Int16:
                    if (obj is short) { _tf64 = JumpTag.Int16; return (short)obj; }
                    goto case JumpTag.Byte;
                case JumpTag.Byte:
                    if (obj is byte) { _tf64 = JumpTag.Byte; return (byte)obj; }
                    goto case JumpTag.Double;
                case JumpTag.Double:
                    if (obj is double) { _tf64 = JumpTag.Double; return (double)obj; }
                    goto case JumpTag.Float;
                case JumpTag.Float:
                    if (obj is float) { _tf64 = JumpTag.Float; return (float)obj; }
                    goto case JumpTag.Decimal;
                case JumpTag.Decimal:
                    if (obj is decimal) { _tf64 = JumpTag.Decimal; return (double)(decimal)obj; }
                    goto case JumpTag.String;
                case JumpTag.String:
                    var str = obj as string;
                    if (str != null) { _tf64 = JumpTag.String; return double.Parse(str, CultureInfo.InvariantCulture); }
                    goto case JumpTag.UInt32;
                case JumpTag.UInt32:
                    if (obj is uint) { _tf64 = JumpTag.UInt32; return (uint)obj; }
                    goto case JumpTag.UInt64;
                case JumpTag.UInt64:
                    if (obj is ulong) { _tf64 = JumpTag.UInt64; return (ulong)obj; }
                    goto case JumpTag.UInt16;
                case JumpTag.UInt16:
                    if (obj is ushort) { _tf64 = JumpTag.UInt16; return (ushort)obj; }
                    goto case JumpTag.SByte;
                case JumpTag.SByte:
                    if (obj is sbyte) { _tf64 = JumpTag.SByte; return (sbyte)obj; }
                    goto case JumpTag.Char;
                case JumpTag.Char:
                    if (obj is char) { _tf64 = JumpTag.Char; return (char)obj; }
                    goto case JumpTag.Invalid;
                case JumpTag.Invalid:
                    if (thru0) goto default;
                    goto case 0;
                default:
                    throw new ArgumentOutOfRangeException(nameof(obj), $"Can't convert {obj} to Double");
            }
        }
        #endregion

        public static double? ToNullableDouble(object obj) => obj == null ? (double?)null : ToDouble(obj);

        #region FastDecimal
        private static JumpTag _tfdec;
        public static decimal ToDecimal(object obj)
        {
            var thru0 = false;
            switch (_tfdec)
            {
                case JumpTag.Int32:
                    thru0 = true;
                    if (obj is int) { _tfdec = JumpTag.Int32; return (int)obj; }
                    goto case JumpTag.Int64;
                case JumpTag.Int64:
                    if (obj is long) { _tfdec = JumpTag.Int64; return (long)obj; }
                    goto case JumpTag.Int16;
                case JumpTag.Int16:
                    if (obj is short) { _tfdec = JumpTag.Int16; return (short)obj; }
                    goto case JumpTag.Byte;
                case JumpTag.Byte:
                    if (obj is byte) { _tfdec = JumpTag.Byte; return (byte)obj; }
                    goto case JumpTag.Double;
                case JumpTag.Double:
                    if (obj is double) { _tfdec = JumpTag.Double; return (decimal)(double)obj; }
                    goto case JumpTag.Float;
                case JumpTag.Float:
                    if (obj is float) { _tfdec = JumpTag.Float; return (decimal)(float)obj; }
                    goto case JumpTag.Decimal;
                case JumpTag.Decimal:
                    if (obj is decimal) { _tfdec = JumpTag.Decimal; return (decimal)obj; }
                    goto case JumpTag.String;
                case JumpTag.String:
                    var str = obj as string;
                    if (str != null) { _tfdec = JumpTag.String; return decimal.Parse(str, CultureInfo.InvariantCulture); }
                    goto case JumpTag.UInt32;
                case JumpTag.UInt32:
                    if (obj is uint) { _tfdec = JumpTag.UInt32; return (uint)obj; }
                    goto case JumpTag.UInt64;
                case JumpTag.UInt64:
                    if (obj is ulong) { _tfdec = JumpTag.UInt64; return (ulong)obj; }
                    goto case JumpTag.UInt16;
                case JumpTag.UInt16:
                    if (obj is ushort) { _tfdec = JumpTag.UInt16; return (ushort)obj; }
                    goto case JumpTag.SByte;
                case JumpTag.SByte:
                    if (obj is sbyte) { _tfdec = JumpTag.SByte; return (sbyte)obj; }
                    goto case JumpTag.Char;
                case JumpTag.Char:
                    if (obj is char) { _tfdec = JumpTag.Char; return (char)obj; }
                    goto case JumpTag.Invalid;
                case JumpTag.Invalid:
                    if (thru0) goto default;
                    goto case 0;
                default:
                    throw new ArgumentOutOfRangeException(nameof(obj), $"Can't convert {obj} to Decimal");
            }
        }
        #endregion

        public static decimal? ToNullableDecimal(object obj) => obj == null ? (decimal?)null : ToDecimal(obj);

        #endregion

        public static Guid ToGuid(object obj)
        {
            if (obj is Guid) return (Guid)obj;
            return Guid.Parse(obj.ToString());
        }

        public static Guid? ToNullableGuid(object obj) => obj == null ? (Guid?)null : ToGuid(obj);

        public static DateTime ToDateTime(object obj) => Convert.ToDateTime(obj);
        public static DateTime? ToNullableDateTime(object obj) => obj == null ? (DateTime?)null : ToDateTime(obj);

        public static DateTimeOffset ToDateTimeOffset(object obj)
        {
            if (obj is DateTimeOffset) return (DateTimeOffset)obj;
            if (obj is DateTime) return (DateTime)obj; // this conversion is evil, but maybe better than nothing
            return DateTimeOffset.Parse(obj.ToString());
        }

        public static DateTimeOffset? ToNullableDateTimeOffset(object obj) =>
            obj == null ? (DateTimeOffset?)null : ToDateTimeOffset(obj);

        public static TimeSpan ToTimeSpan(object obj)
        {
            if (obj is TimeSpan) return (TimeSpan)obj;
            return TimeSpan.Parse(obj.ToString());
        }

        public static TimeSpan? ToNullableTimeSpan(object obj) => obj == null ? (TimeSpan?)null : ToTimeSpan(obj);

        private static readonly Dictionary<Type, MethodInfo> Converters = 
            typeof(PrimitiveConverter)
                .GetMethods(BindingFlags.Public | BindingFlags.Static)
                .Where(m =>
                {
                    var pars = m.GetParameters();
                    return pars.Length == 1 && pars[0].ParameterType == typeof(object);
                }).ToDictionary(m => m.ReturnType);

        public static bool IsPrimitive(Type targetType) => Converters.ContainsKey(targetType);

        public static MethodInfo ToType(Type targetType)
        {
            MethodInfo converter;
            if (Converters.TryGetValue(targetType, out converter)) return converter;
            throw new NotSupportedException($"Can't convert to {targetType}");
        }
    }
}