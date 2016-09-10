module Rezoom.ORM.SQLProvider.Language
open System
open System.Collections.Generic

type ILanguageStatement =
    abstract member ModelChange : IModel option
    abstract member ResultSets : ISchemaQuery IReadOnlyList
    abstract member TablesRead : ISchemaTable IReadOnlyList
    abstract member TablesWritten : ISchemaTable IReadOnlyList
    abstract member Parameters : (BindParameter * ColumnType) IReadOnlyList

let nullStatement =
    { new ILanguageStatement with
        member __.ModelChange = None
        member __.ResultSets = upcast [||]
        member __.TablesRead = upcast [||]
        member __.TablesWritten = upcast [||]
        member __.Parameters = upcast [||]
    }

let languageStatement (input : IModel) (stmt : Stmt) =
    match stmt with
    | AlterTableStmt alter -> failwith "not implemented"
    | AnalyzeStmt objectName -> nullStatement
    | AttachStmt (attach, name) -> failwith "not implemented"
    | BeginStmt transaction -> nullStatement
    | CommitStmt -> nullStatement
    | CreateIndexStmt create -> failwith "not implemented"
    | CreateTableStmt create -> failwith "not implemented"
    | CreateTriggerStmt create -> failwith "not implemented"
    | CreateViewStmt create -> failwith "not implemented"
    | CreateVirtualTableStmt _ -> nullStatement
    | DeleteStmt delete -> failwith "not implemented"
    | DetachStmt detatch -> failwith "not implemented"
    | DropObjectStmt drop -> failwith "not implemented"
    | InsertStmt insert -> failwith "not implemented"
    | PragmaStmt pragma -> nullStatement
    | ReindexStmt objectName -> failwith "not implemented"
    | ReleaseStmt name -> nullStatement
    | RollbackStmt rollback -> nullStatement
    | SavepointStmt name -> nullStatement
    | SelectStmt select -> failwith "not implemented"
    | ExplainStmt stmt -> nullStatement
    | UpdateStmt update -> failwith "not implemented"
    | VacuumStmt -> nullStatement