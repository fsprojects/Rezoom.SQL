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

/// 2D point as a user primitive that stores as PG `point`. Where Address
/// exercises an obj-underlying type whose value carries the column data as
/// a string, Point2D exercises an obj-underlying type whose value is a
/// driver-specific struct (NpgsqlPoint) — Npgsql's native CLR
/// representation for the `point` backend type. This proves the
/// System.Object escape hatch also handles non-string driver values.
// Note: 15 = NpgsqlTypes.NpgsqlDbType.Point (Npgsql 8.x). Hardcoded
// as a literal for the same attribute-argument reason as Jsonb above.
[<RawBackendSQLType("point")>]
[<SQLParameterDbType("NpgsqlDbType", 15)>]
type Point2D =
    {   X : double
        Y : double
    }
    static member ToPrimitive(p : Point2D) : obj =
        box (NpgsqlTypes.NpgsqlPoint(p.X, p.Y))
    static member FromPrimitive(o : obj) : Point2D =
        let pt = o :?> NpgsqlTypes.NpgsqlPoint
        { X = pt.X; Y = pt.Y }
