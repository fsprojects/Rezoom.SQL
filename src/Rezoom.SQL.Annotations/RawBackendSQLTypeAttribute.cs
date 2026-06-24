using System;
namespace Rezoom.SQL.Annotations;

/// <summary>
/// Overrides the literal backend SQL type string Rezoom.SQL should emit
/// for a user-defined primitive. The value is treated as opaque.
/// Length parameters, precision, etc. must be included in the string,
/// since Rezoom.SQL does not parse or modify it. It is your own job
/// to ensure the underlying primitive you chose is compatible,
/// (e.g. string for a CHAR fixed-length col).
///
/// May be placed either on the type (e.g. a
/// single-case discriminated union) or on the
/// <c>ToPrimitive</c> or <c>FromPrimitive</c> method.
/// </summary>
[AttributeUsage
    ( AttributeTargets.Class | AttributeTargets.Struct | AttributeTargets.Method
    , AllowMultiple = false
    , Inherited = false
    )]
public sealed class RawBackendSQLTypeAttribute : Attribute
{
    public RawBackendSQLTypeAttribute(string sqlType)
    {
        SqlType = sqlType;
    }

    public string SqlType { get; }
}
