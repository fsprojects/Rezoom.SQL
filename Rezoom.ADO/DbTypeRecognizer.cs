using System;
using System.Collections.Generic;
using System.Data;

namespace Rezoom.ADO
{
    public class DbTypeRecognizer : IDbTypeRecognizer
    {
        public virtual DbType StringType => DbType.AnsiString;
        public virtual DbType DateTimeType => DbType.DateTime2;
        private static readonly Dictionary<Type, DbType> Primitives = new Dictionary<Type, DbType>
        {
            { typeof(bool), DbType.Boolean },

            { typeof(long), DbType.Int64 },
            { typeof(int), DbType.Int32 },
            { typeof(short), DbType.Int16 },
            { typeof(sbyte), DbType.SByte },

            { typeof(ulong), DbType.UInt64 },
            { typeof(uint), DbType.UInt32 },
            { typeof(ushort), DbType.UInt16 },
            { typeof(byte), DbType.Byte },

            { typeof(double), DbType.Double },
            { typeof(float), DbType.Single },
            { typeof(decimal), DbType.Decimal },

            { typeof(Guid), DbType.Guid },
        };
        public DbType GetDbType(object value)
        {
            if (value == null) return DbType.Object;
            if (value is string) return StringType;
            if (value is DateTime) return DateTimeType;
            var type = value.GetType();
            DbType result;
            if (Primitives.TryGetValue(type, out result)) return result;
            throw new NotSupportedException($"The type {type} is not supported for a database parameter");
        }
    }
}