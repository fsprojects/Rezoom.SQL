namespace TypeProviderUser.Postgres.UserTypes

open System.Text.Json
open Rezoom.SQL.Annotations

/// Address as a user primitive that stores as PG jsonb. Demonstrates the
/// System.Object underlying-CLR-type escape hatch: ToPrimitive returns
/// `obj` (a JSON-serialized string boxed), and FromPrimitive accepts the
/// same `obj` shape coming back from the driver. The RawBackendSQLType
/// pins the SQL type as "jsonb" and the ParameterDbType attribute tells
/// the runtime to set NpgsqlDbType.Jsonb on the parameter so Npgsql
/// binds it as the right backend type.
// Note: 36 = NpgsqlTypes.NpgsqlDbType.Jsonb (Npgsql 8.x).
// F# does not accept enum-to-int casts in attribute argument
// position so the integer literal is the cleanest available form.
[<RawBackendSQLType("jsonb")>]
[<SQLParameterDbType("NpgsqlDbType", 36)>]
type Address =
    {   Street : string
        City : string
        State : string
        Zip : string
    }
    static member ToPrimitive(a : Address) : obj =
        box (JsonSerializer.Serialize(a))
    static member FromPrimitive(o : obj) : Address =
        JsonSerializer.Deserialize<Address>(o :?> string)
