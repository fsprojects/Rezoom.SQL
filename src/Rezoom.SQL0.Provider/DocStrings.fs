module private Rezoom.SQL.Provider.DocStrings
open Rezoom.SQL.Compiler

let private shortDescriptionCore (statement : Stmt<_, _>) =
    match statement with
    | AlterTableStmt _ -> "ALTER TABLE"
    | CreateIndexStmt _ -> "CREATE INDEX"
    | CreateTableStmt _ -> "CREATE TABLE"
    | CreateViewStmt _ -> "CREATE VIEW"
    | DeleteStmt _ -> "DELETE"
    | DropObjectStmt { Drop = DropIndex } -> "DROP INDEX"
    | DropObjectStmt { Drop = DropTable } -> "DROP TABLE"
    | DropObjectStmt { Drop = DropView } -> "DROP VIEW"
    | InsertStmt _ -> "INSERT"
    | SelectStmt _ -> "SELECT"
    | UpdateStmt _ -> "UPDATE"

let private shortDescription (statement : TotalStmt<_, _>) =
    match statement with
    | CoreStmt st -> shortDescriptionCore st
    | VendorStmt v ->
        let vendor = "VENDOR {...}"
        match v.ImaginaryStmts with
        | None -> vendor
        | Some stmts ->
            vendor + " IMAGINE {" + String.concat ", " [ for stmt in stmts -> shortDescriptionCore stmt ] + "}"
    

let commandEffectDocString (commandEffect : CommandEffect) =
    let cacheInfo =
        match commandEffect.CacheInfo.Value with
        | None -> "Read/write behavior unknown - assumed to invalidate all table caches."
        | Some info ->
            [   if info.Idempotent then
                    yield "Idempotent."
                else 
                    yield "Non-idempotent."
                if info.ReadTables.Count > 0 then
                    yield
                        "Reads from tables: "
                        + (info.ReadTables |> Seq.map (fun t -> string t.ObjectName) |> String.concat ", ")
                        + "."
                if info.WriteTables.Count > 0 then
                    yield
                        "Writes to tables: "
                        + (info.WriteTables |> Seq.map (fun t -> string t.ObjectName) |> String.concat ", ")
                        + "."     
            ] |> String.concat " "
    let statementDescrs =
        commandEffect.Statements
        |> Seq.map shortDescription
        |> String.concat "; "
    let parameters =
        [ for parameter, columnType in commandEffect.Parameters ->
            string parameter + " : " + string columnType
        ] |> String.concat ", "
    [   yield statementDescrs
        if parameters = "" then
            yield "(unparameterized)."
        else
            yield "(" + parameters + ")."
        yield cacheInfo
    ] |> String.concat " "

