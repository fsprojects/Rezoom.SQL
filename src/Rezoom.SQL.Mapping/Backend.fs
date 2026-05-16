namespace Rezoom.SQL.Mapping
open System

/// Compile-time dialect identifier. The rzsql.json parser turns its `"backend"`
/// string into one of these; downstream consumers use the DU instead of passing
/// strings around. Set on `CommandData` by the TP and passed to
/// `ConnectionProvider.Open` so a configuration-driven provider can pick the
/// right ADO.NET driver per dialect.
type Backend =
    /// No translation. Outputs RZSQL syntax. Used for the "rzsql" no-op backend.
    | RzSQL
    | SQLite
    | TSQL
    | Postgres

module Backend =
    /// Parse the string form used in rzsql.json's `backend` setting.
    /// Case-insensitive. Returns None for unknown values so the caller can
    /// produce a parse error with location info.
    let parse (s : string) : Backend option =
        if isNull s then None else
        match s.ToLowerInvariant() with
        | "rzsql" -> Some RzSQL
        | "sqlite" -> Some SQLite
        | "tsql" | "mssql" -> Some TSQL
        | "postgres" | "postgresql" -> Some Postgres
        | _ -> None

    /// Canonical ADO.NET provider invariant for this dialect. Matches the driver
    /// shipped by the corresponding Rezoom.SQL.Provider.{SQLite,TSQL,Postgres}
    /// meta-package. For the no-op RzSQL backend, falls through to TSQL's driver
    /// as a placeholder; actually trying to run that dialect against a real
    /// database is a configuration error.
    let canonicalDriver = function
        | RzSQL -> "Microsoft.Data.SqlClient"
        | SQLite -> "Microsoft.Data.Sqlite"
        | TSQL -> "Microsoft.Data.SqlClient"
        | Postgres -> "Npgsql"
