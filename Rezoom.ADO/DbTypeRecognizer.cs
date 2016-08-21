using System;
using System.Collections.Generic;
using System.Data;

namespace Rezoom.ADO
{
    public class DbTypeRecognizer : IDbTypeRecognizer
    {
        public virtual DbType StringType => DbType.String;
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
        public virtual void GetDbType(ref object value, out DbType dbType)
        {
            if (value == null) { dbType = DbType.Object; return; }
            if (value is string) { dbType = StringType; return; }
            if (value is DateTime) { dbType = DateTimeType; return; }
            var ty = value.GetType();
            if (Primitives.TryGetValue(ty, out dbType)) return;
            throw new NotSupportedException($"The type {dbType} is not supported for a database parameter");
        }
    }
}