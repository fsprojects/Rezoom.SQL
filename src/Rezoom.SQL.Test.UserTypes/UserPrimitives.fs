namespace Rezoom.SQL.Test.UserTypes

open Rezoom.SQL.Annotations

// Sample user primitives that we can test materializing from Rezoom.SQL.Mapping!
// The implementations here are trashy placeholders with no error checking, no validation!
// We just care about testing the mapping calls the right methods.

/// Has annotation attribute  for testing.
[<RawBackendSQLType("MEDIUMINT")>]
type CompactInt = CompactInt of int

/// Has annotation attribute for testing.
[<SQLTypeLength(80)>]
type ShortName = ShortName of string

/// A type that declares its own ToPrimitive and FromPrimitive.
type StringyPhoneNumber =
    {   CountryCode : int
        AreaCode : int
        Number : int
    }
    static member ToPrimitive(phone : StringyPhoneNumber) =
        sprintf "+%1d%03d%07d" phone.CountryCode phone.AreaCode phone.Number
    static member FromPrimitive(str : string) =
        {   CountryCode = int (str.[1..1])
            AreaCode = int (str.[2..4])
            Number = int (str.[5..])
        }

/// ToPrimitive and FromPrimitive will be declared as extensions in the consumer test assembly.
/// This should also work.
type IntyPhoneNumber(countryCode : int, areaCode : int, num : int) =
    member this.CountryCode = countryCode
    member this.AreaCode = areaCode
    member this.Number = num

///// Should fail because it has two FromPrimitive definitions.
///// Need to have a separate assembly so we can test this failure without *all* the custom types tests failing
///// at assembly-scan-time.
//type TooManyConverters() =
//    static member ToPrimitive(x : TooManyConverters) = 0
//    static member FromPrimitive(x : int) = TooManyConverters()
//    static member FromPrimitive(x : string) = TooManyConverters()

/// Testing that we can add primitive converters for types beyond our control,
/// and could even override the default Rezoom.SQL.Mapping handling to remap a primitive
/// it supports natively like DateTimeOffset and represent it as a string instead.
module ExtendingSystemPrimitives =
    // F#-style extension methods can be used.
    type System.DateTimeOffset with
        member this.ToPrimitive() = this.ToString("o")
        static member FromPrimitive(x : string) = System.DateTimeOffset.Parse(x)
    
    // F# let-bound functiosn can be used, as long as they are PascalCase.
    let ToPrimitive (ts : System.TimeSpan) = ts.Ticks
    let FromPrimitive (ticks : int64) = System.TimeSpan.FromTicks(ticks)

// Static methods found any-which-where can be used
type AdhocExtensionClass() =
    static member ToPrimitive(t : System.TimeOnly) = t.ToString("o")
    static member FromPrimitive(str : string) = System.TimeOnly.ParseExact(str, "o")

// Single-case DUs, both struct and ref, should work without any annotation

type CustomUserId = CustomUserId of System.Guid

type CustomStringId = CustomStringId of string

[<Struct>]
type CustomUserIdStruct = CustomUserIdStruct of System.Guid

[<Struct>]
type CustomStringIdStruct = CustomStringIdStruct of string

type EmailAddress = EmailAddress of string

[<Struct>]
type UserId = UserId of int

// Overriding built-in CLR types with alternative representations

module GuidOverrideExtension =
    type System.Guid with
        [<RawBackendSQLType("CHAR(36)")>]
        member this.ToPrimitive() = this.ToString()
        static member FromPrimitive(s : string) = System.Guid.Parse(s)

/// Single-case DU over byte[] with no length specified, acts
/// like BinaryTypeName(None).
type FileHash = FileHash of byte[]

/// Single-case DU over byte[] with [<SQLTypeLength>] acts like
/// BinaryTypeName(Some 32).
[<SQLTypeLength(32)>]
type ShortHash = ShortHash of byte[]

// --- SQLParameterDbType fixtures --------------------------------------
// These exist purely so the loader-inspection tests in
// Rezoom.SQL.Test.TestUserTypeAnnotations can assert the
// SQLParameterDbType attribute round-trips into UserPrimitiveType. The
// types themselves are never bound at runtime in the Rezoom.SQL.Test
// suite; the assertions read the loaded UserTypeLibrary directly.

/// Exercises the single-arg constructor — SQLParameterDbType(DbType).
/// Expected to surface as ("DbType", int DbType.AnsiString) on the
/// loaded UserPrimitiveType.SQLParameterDbType.
[<SQLParameterDbType(System.Data.DbType.AnsiString)>]
type AnsiLabel = AnsiLabel of string

/// Exercises the two-arg escape-hatch constructor for provider-specific
/// enums — SQLParameterDbType(propertyName, int). The values here are
/// deliberately not bound at runtime; we only care that the metadata
/// round-trips. (36 happens to be NpgsqlDbType.Jsonb but no
/// Postgres-specific assembly is referenced from this fixture.)
[<SQLParameterDbType("NpgsqlDbType", 36)>]
type OpaqueDbTypeProbe = OpaqueDbTypeProbe of string