using System;
using System.Data;
using System.Data.Common;
namespace Rezoom.SQL.Annotations;

/// <summary>
/// Overrides the DbType to use when this user-primitive is passed as a parameter to a command.
/// Particularly useful when the user-primitive is mapped to System.Object, so we can't automatically infer
/// an appropriate DbType for the parameter.
/// 
/// Can be placed on the mapped type or on either of the FromPrimitive/ToPrimitive methods.
/// </summary>
[AttributeUsage
    ( AttributeTargets.Class | AttributeTargets.Struct | AttributeTargets.Method
    , AllowMultiple = false
    , Inherited = false
    )]
public sealed class SQLParameterDbTypeAttribute : Attribute
{
    public SQLParameterDbTypeAttribute(DbType dbType) : this(nameof(DbParameter.DbType), (int)dbType)
    {
    }
    /// <summary>
    /// Use this constructor when your backend has its own special DbParameter type like "NpgsqlDbType" that's found outside of System.Data.
    /// </summary>
    /// <param name="dbParameterPropertyName"></param>
    /// <param name="value"></param>
    public SQLParameterDbTypeAttribute(string dbParameterPropertyName, int value)
    {
        DbParameterPropertyName = dbParameterPropertyName;
        Value = value;
    }
    public string DbParameterPropertyName { get; }

    public int Value { get; }
}
