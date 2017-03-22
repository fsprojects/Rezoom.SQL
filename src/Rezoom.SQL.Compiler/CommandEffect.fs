// A command is a series of SQL statements.
// This module analyzes the effects of commands, including the tables they update, the changes they make to the model,
// and the result sets they output.
namespace Rezoom.SQL.Compiler
open System
open System.Collections.Generic
open Rezoom.SQL.Compiler.InferredTypes

type CommandEffectCacheInfo =
    {   Idempotent : bool
        // schema name * table name
        WriteTables : (Name * Name) IReadOnlyList
        ReadTables : (Name * Name) IReadOnlyList
    }

type CommandEffect =
    {   Statements : TTotalStmt IReadOnlyList
        Parameters : (BindParameter * ColumnType) IReadOnlyList
        ModelChange : Model option
        DestructiveUpdates : bool Lazy
        CacheInfo : CommandEffectCacheInfo option Lazy // if we have any vendor stmts this is unknown
    }
    member this.ResultSets() =
        this.Statements
        |> Seq.collect (fun s -> s.SelectStmts())
        |> Seq.map (fun s -> s.Value.Info.Table.Query)
    static member ParseSQL(descr: string, sql : string) : TotalStmts =
        Parser.parseStatements descr sql |> toReadOnlyList
    static member OfSQL(model : Model, stmts : TotalStmts) =
        let builder = CommandEffectBuilder(model)
        for stmt in stmts do
            builder.AddTotalStmt(stmt)
        builder.CommandEffect()
    static member OfSQL(model : Model, descr : string, sql : string) =
        catchSource descr sql <| fun () ->
            let stmts = CommandEffect.ParseSQL(descr, sql)
            CommandEffect.OfSQL(model, stmts)

and private CommandEffectBuilder(model : Model) =
    // shared throughout the whole command, since parameters are too.
    let inference = TypeInferenceContext() :> ITypeInferenceContext
    let inferredStmts = ResizeArray()
    let mutable newModel = None
    member private this.AddStmt(stmt : Stmt) =
        let model = newModel |? model
        let checker = TypeChecker(inference, InferredSelectScope.Root(model))
        let inferredStmt = checker.Stmt(stmt)
        newModel <- ModelChange(model, inference).Stmt(inferredStmt)
        inferredStmt
    member this.AddTotalStmt(stmt : TotalStmt) =
        match stmt with
        | CoreStmt stmt -> this.AddStmt(stmt) |> CoreStmt |> inferredStmts.Add
        | VendorStmt vendor ->
            let model = newModel |? model
            let checker = TypeChecker(inference, InferredSelectScope.Root(model))
            let frag = function
                | VendorEmbeddedExpr e -> VendorEmbeddedExpr (checker.Expr(e))
                | VendorRaw str -> VendorRaw str
            let checkedFrags = vendor.Fragments |> rmap frag
            let checkedImaginary = vendor.ImaginaryStmts |> Option.map (rmap this.AddStmt)
            {   VendorName = vendor.VendorName
                Fragments = checkedFrags
                ImaginaryStmts = checkedImaginary
            } |> VendorStmt |> inferredStmts.Add

    static member PerformsDestructiveUpdate(stmt : TStmt) =
        match stmt with
        | AlterTableStmt { Alteration = AddColumn _ }
        | CreateIndexStmt _
        | CreateTableStmt _
        | SelectStmt _
        | BeginStmt
        | CommitStmt
        | RollbackStmt
        | CreateViewStmt _ -> false
        | AlterTableStmt { Alteration = RenameTo _ }
        | DeleteStmt _
        | DropObjectStmt _
        | InsertStmt _
        | UpdateStmt _ -> true

    static member PerformsDestructiveUpdate(stmt : TTotalStmt) =
        match stmt with
        | CoreStmt core -> CommandEffectBuilder.PerformsDestructiveUpdate(core)
        | VendorStmt { ImaginaryStmts = Some stmts } ->
            stmts |> Seq.exists CommandEffectBuilder.PerformsDestructiveUpdate
        | VendorStmt { ImaginaryStmts = None } -> false

    member this.CommandEffect() =
        let mapping = concreteMapping inference
        let stmts = inferredStmts |> Seq.map mapping.TotalStmt |> toReadOnlyList
        let pars =
            inference.Parameters
            |> Seq.map (fun p -> p, inference.Concrete(inference.Variable(p)))
            |> toReadOnlyList
        let cacheInfo =
            lazy (
                let vendorStmts = stmts |> Seq.choose (function | VendorStmt v -> Some v | _ -> None)
                if vendorStmts |> Seq.forall (fun v -> Option.isSome v.ImaginaryStmts) then
                    let references = ReadWriteReferences.references (stmts |> Seq.collect (fun s -> s.CoreStmts()))
                    let toTuple (ref : SchemaTable) = ref.SchemaName, ref.TableName
                    let inline selectsIdempotent() =
                        stmts
                        |> Seq.collect (fun s -> s.SelectStmts())
                        |> Seq.forall (fun s -> s.Value.Info.Idempotent)
                    {   WriteTables = references.TablesWritten |> Seq.map toTuple |> toReadOnlyList
                        ReadTables = references.TablesRead |> Seq.map toTuple |> toReadOnlyList
                        Idempotent = references.TablesWritten.Count <= 0 && selectsIdempotent()
                    } |> Some
                else
                    None
            )
        let destructive = lazy (stmts |> Seq.exists CommandEffectBuilder.PerformsDestructiveUpdate)
        {   Statements = stmts
            ModelChange = newModel
            Parameters = pars
            DestructiveUpdates = destructive
            CacheInfo = cacheInfo
        }