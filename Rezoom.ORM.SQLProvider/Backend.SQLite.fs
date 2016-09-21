namespace Rezoom.ORM.SQLProvider
open System
open System.Collections.Generic
open Rezoom.ORM
open SQLow

type SQLiteTranslator(indexer : IParameterIndexer) =
    static let sep separator sequence =
        seq {
            let separator = CommandText separator
            let mutable first = true
            for element in sequence do
                if not first then yield separator
                else first <- false
                yield! element
        }
    static let sep1 separator sequence = sep separator (sequence |> Seq.map Seq.singleton)
    static let t str = CommandText str
    static let loc (name : Name) = LocalName name.Value
    static let space = t " "
    let p bindParam = Parameter (indexer.ParameterIndex bindParam)
    member this.Name(name : Name) = CommandText ("[" + name.Value + "]")
    member this.Translate(cte : CommonTableExpression) =
        seq {
            yield this.Name(cte.Name)
            yield space
            match cte.ColumnNames with
            | None -> ()
            | Some names ->
                yield t "("
                yield! names.Value |> Seq.map this.Name |> sep1 ", "
                yield t ") "
            yield t "AS ("
            yield! this.Translate(cte.AsSelect)
            yield t ")"
        }
    member this.Translate(withClause : WithClause) =
        seq {
            yield t "WITH "
            if withClause.Recursive then yield t "RECURSIVE "
            for cte in withClause.Tables do
                yield! this.Translate(cte)
        }
    member this.Translate(objectName : ObjectName) =
        seq {
            match objectName.SchemaName with
            | None -> ()
            | Some schemaName ->
                yield this.Name(schemaName)
                yield t "."
            yield this.Name(objectName.ObjectName)
        }
    member this.Translate(expr : Expr) =
        failwith "not implemented"
    member this.Translate(column : ResultColumn WithSource) =
        match column.Value with
        | ColumnsWildcard -> t "*" |> Seq.singleton
        | TableColumnsWildcard name ->
            seq {
                yield! this.Translate(name)
                yield t ".*"
            }
        | Column (expr, alias) ->
            seq {
                yield! this.Translate(expr)
                match alias with
                | None -> ()
                | Some alias ->
                    yield t " AS "
                    yield this.Name(alias)
            }
    member this.Translate(columns : ResultColumns) =
        seq {
            match columns.Distinct with
            | None
            | Some AllColumns -> ()
            | Some DistinctColumns -> yield t "DISTINCT "
            yield! columns.Columns |> Seq.map this.Translate |> sep ", "
        }
    member this.Translate(select : SelectCore) =
        seq {
            yield t "SELECT "
            yield! this.Translate(select.Columns)
        }
    member this.Translate(term : CompoundTerm) =
        match term.Value with
        | Values values -> failwith ""
        | Select selectCore -> this.Translate(selectCore)
    member this.Translate(compound : CompoundExpr) =
        let op name (expr : CompoundExpr) (term : CompoundTerm) =
            seq {
                yield! this.Translate(expr)
                yield space
                yield t name
                yield space
                yield! this.Translate(term)
            }
        match compound.Value with
        | CompoundTerm term -> this.Translate(term)
        | Union (expr, term) -> op "UNION" expr term
        | UnionAll (expr, term) -> op "UNION ALL" expr term
        | Intersect (expr, term) -> op "INTERSECT" expr term
        | Except (expr, term) -> op "EXCEPT" expr term
    member this.Translate(select : SelectStmt) =
        let select = select.Value
        seq {
            match select.With with
            | None -> ()
            | Some withClause ->
                yield! this.Translate(withClause)
                yield space
            yield! this.Translate(select.Compound)
            match select.OrderBy with
            | None -> ()
            | Some orderBy -> failwith "not implemented"
            match select.Limit with
            | None -> ()
            | Some limit -> failwith "not implemented"
        }
    member this.Translate(stmt : Stmt) =
        match stmt with
        | SelectStmt select -> this.Translate(select)
        | _ -> failwithf "Unimplemented statement %.1A" stmt
    member this.Translate(stmts : Stmt seq) =
        seq {
            for stmt in stmts do yield! this.Translate(stmt)
        }

type SQLiteBackend() =
    interface IBackend with
        member this.Builtin =
            {   Functions = Map.empty
            }
        member this.MapPrimitiveType(ty) =
            match ty.Type with
            | IntegerType -> if ty.Nullable then typeof<Nullable<int64>> else typeof<int64>
            | BooleanType -> if ty.Nullable then typeof<Nullable<bool>> else typeof<bool>
            | FloatType -> if ty.Nullable then typeof<Nullable<double>> else typeof<double>
            | StringType -> typeof<string>
            | BlobType -> typeof<byte array>
            | AnyType -> typeof<obj>
        member this.ToCommandFragments(indexer, stmts) =
            let translator = SQLiteTranslator(indexer)
            translator.Translate(stmts)
            |> BackendUtilities.simplifyFragments
            |> ResizeArray
            :> _ IReadOnlyList
       