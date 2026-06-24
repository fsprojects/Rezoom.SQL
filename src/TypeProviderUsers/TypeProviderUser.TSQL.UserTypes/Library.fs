namespace TypeProviderUser.TSQL.UserTypes

open System.Text.Json
open Rezoom.SQL.Annotations

/// Address as a user primitive that stores as TSQL `json` (SQL Server
/// 2025+ native type). Same shape as the Postgres jsonb Address fixture:
/// ToPrimitive serializes to a JSON string, FromPrimitive deserializes
/// from one. The RawBackendSQLType pins the SQL column type as "json".
///
/// Note on SQLParameterDbType: SqlDbType.Json (= 35) exists in
/// System.Data but Microsoft.Data.SqlClient 5.2.2 rejects it as
/// "invalid" when assigned to a SqlParameter. Until SqlClient catches
/// up we bind as NVarChar (= 12); SQL Server implicitly converts an
/// nvarchar parameter value to json when assigning to a json column.
// 12 = System.Data.SqlDbType.NVarChar
[<RawBackendSQLType("json")>]
[<SQLParameterDbType("SqlDbType", 12)>]
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

/// 2D geographic location as a user primitive that stores as TSQL
/// `geography`. Same intent as the Postgres Point2D fixture, but the
/// in-flight CLR shape is asymmetric: parameter binding goes through
/// nvarchar carrying WKT (SQL Server auto-converts to geography on
/// INSERT), while reads come back as a SqlGeography UDT instance
/// (which Microsoft.Data.SqlClient deserializes for any geography
/// column). FromPrimitive consequently has to know how to unpack a
/// SqlGeography.
///
/// Why not UDT-bind directly? Setting up a SqlParameter for a UDT
/// requires both SqlDbType.Udt (= 29) AND the UdtTypeName property
/// ("geography"). SQLParameterDbType is a one-property attribute; the
/// nvarchar+server-conversion path sidesteps that.
// 12 = System.Data.SqlDbType.NVarChar
[<RawBackendSQLType("geography")>]
[<SQLParameterDbType("SqlDbType", 12)>]
type GeoLocation =
    {   Latitude : double
        Longitude : double
    }
    static member ToPrimitive(g : GeoLocation) : obj =
        // SRID 4326 (WGS84) — same coordinate system the read side
        // assumes. The WKT lon-lat order is intentional: SQL Server
        // STGeomFromText interprets POINT(x y) as POINT(lon lat).
        box (System.String.Format(System.Globalization.CultureInfo.InvariantCulture,
                "POINT({0} {1})", g.Longitude, g.Latitude))
    static member FromPrimitive(o : obj) : GeoLocation =
        let sg = o :?> Microsoft.SqlServer.Types.SqlGeography
        { Latitude = sg.Lat.Value; Longitude = sg.Long.Value }
