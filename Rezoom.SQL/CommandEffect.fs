// A command is a series of SQL statements.
// This module analyzes the effects of commands, including the tables they update, the changes they make to the model,
// and the result sets they output.
namespace Rezoom.SQL
open System
open System.Collections.Generic
open Rezoom.SQL.InferredTypes

type CommandEffect =
    {   Statements : TTotalStmt IReadOnlyList
        Parameters : (BindParameter * ColumnType) IReadOnlyList
        ModelChange : Model option
        Idempotent : bool
        WriteTables : (Name * Name) seq
        ReadTables : (Name * Name) seq
    }
    member this.ResultSets =
        this.Statements
        |> Seq.collect (fun s -> s.SelectStmts())
        |> Seq.map (fun s -> s.Value.Info.Table.Query)
    static member ParseSQL(descr: string, sql : string) : Stmts =
        Parser.parseStatements descr sql |> toReadOnlyList
    static member OfSQL(model : Model, stmts : Stmts) =
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

    member this.CommandEffect() =
        let mapping = concreteMapping inference
        let stmts = inferredStmts |> Seq.map mapping.TotalStmt |> toReadOnlyList
        let pars =
            inference.Parameters
            |> Seq.map (fun p -> p, inference.Concrete(inference.Variable(p)))
            |> toReadOnlyList
        let references = ReadWriteReferences.references (stmts |> Seq.collect (fun s -> s.CoreStmts()))
        let inline selectsIdempotent() =
            stmts
            |> Seq.collect (fun s -> s.SelectStmts())
            |> Seq.forall (fun s -> s.Value.Info.Idempotent)
        {   Statements = stmts
            ModelChange = newModel
            Parameters = pars
            WriteTables =
                seq {
                    for ref in references.TablesWritten -> ref.SchemaName, ref.TableName
                }
            ReadTables =
                seq {
                    for ref in references.TablesRead -> ref.SchemaName, ref.TableName
                }
            Idempotent = references.TablesWritten.Count <= 0 && selectsIdempotent()
        }