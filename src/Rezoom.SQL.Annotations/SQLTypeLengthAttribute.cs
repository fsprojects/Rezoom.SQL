using System;
namespace Rezoom.SQL.Annotations;

/// <summary>
/// Declares a length parameter to apply to the backend-default SQL type
/// for a user-defined primitive. Used when you want the backend's
/// usual mapping (e.g. <c>string</c> -&gt; <c>NVARCHAR</c> /
/// <c>VARCHAR</c>) but with a specific max-length.
///
/// Mutually exclusive with <see cref="RawBackendSQLTypeAttribute"/> on
/// the same primitive, because if RawBackendSQLType is specified then this would be ignored.
///
/// May be placed either on the type or on the
/// <c>ToPrimitive</c> or <c>FromPrimitive</c> method.
/// </summary>
[AttributeUsage
    (AttributeTargets.Class | AttributeTargets.Struct | AttributeTargets.Method
    , AllowMultiple = false
    , Inherited = false
    )]
public sealed class SQLTypeLengthAttribute : Attribute
{
    public SQLTypeLengthAttribute(int length)
    {
        Length = length;
    }

    public int Length { get; }
}
