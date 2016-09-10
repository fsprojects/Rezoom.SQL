module Rezoom.ORM.SQLProvider.Language
open System
open System.Collections.Generic
open Rezoom.ORM.SQLProvider.InferredTypes

type LanguageStatement =
    {
        ModelChange : IModel option
        ResultSets : ISchemaQuery IReadOnlyList
        TablesRead : ISchemaTable IReadOnlyList
        TablesWritten : ISchemaTable IReadOnlyList
        Parameters : (BindParameter * ColumnType) IReadOnlyList
    }


let nullStatement =
    {
        ModelChange = None
        ResultSets = [||] :> _ IReadOnlyList
        TablesRead = [||] :> _ IReadOnlyList
        TablesWritten = [||] :> _ IReadOnlyList
        Parameters = [||] :> _ IReadOnlyList
    }

let selectStatement model select = // TODO: primary key-ness
    let cxt = TypeCheckerContext()
    let checker = TypeChecker(cxt, InferredSelectScope.Root(model))
    let query = checker.InferQueryType(select)
    let tables = cxt.References |> toReadOnlyList
    let mutable query = Unchecked.defaultof<ISchemaQuery>
    let columns =
        seq {
            for col in query.Columns ->
                { new ISchemaQueryColumn with
                    member __.Query = query
                    member __.ColumnName = col.ColumnName
                    member __.ColumnType = col.ColumnType
                }
        } |> toReadOnlyList
    let columnsByName = lazy (columns |> ciDictBy (fun c -> c.ColumnName))
    query <-
        { new ISchemaQuery with
            member __.Columns = columns
            member __.ColumnsByName = columnsByName.Value
            member __.ReferencedTables = upcast tables
        }
    { nullStatement with
        TablesRead = tables
        Parameters = cxt.Parameters |> toReadOnlyList
        ResultSets = [| query |] :> _ IReadOnlyList
    }

let languageStatement (model : IModel) (stmt : Stmt) =
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
    | SelectStmt select -> selectStatement model select
    | ExplainStmt stmt -> nullStatement
    | UpdateStmt update -> failwith "not implemented"
    | VacuumStmt -> nullStatement