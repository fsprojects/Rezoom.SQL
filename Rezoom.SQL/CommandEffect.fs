// A command is a series of SQL statements.
// This module analyzes the effects of commands, including the tables they update, the changes they make to the model,
// and the result sets they output.
namespace Rezoom.SQL
open System
open System.Collections.Generic
open Rezoom.SQL.InferredTypes

type CommandEffect =
    {   Statements : TStmt IReadOnlyList
        Parameters : (BindParameter * ColumnType) IReadOnlyList
        ModelChange : Model option
        WriteTables : (Name * Name) seq
        ReadTables : (Name * Name) seq
    }
    member this.ResultSets =
        seq {
            for stmt in this.Statements do
                match stmt with
                | SelectStmt stmt -> yield stmt.Value.Info.Table.Query
                | _ -> ()
        }
    static member None =
        {   Statements = [||] :> _ IReadOnlyList
            Parameters = [||] :> _ IReadOnlyList
            ModelChange = None
            WriteTables = Seq.empty
            ReadTables = Seq.empty
        }
    static member ParseSQL(descr: string, sql : string) : Stmts =
        Parser.parseStatements descr sql |> toReadOnlyList
    static member OfSQL(model : Model, stmts : Stmts) =
        let builder = CommandEffectBuilder(model)
        for stmt in stmts do
            builder.AddStatement(stmt)
        builder.CommandEffect()
    static member OfSQL(model : Model, descr : string, sql : string) =
        let stmts = CommandEffect.ParseSQL(descr, sql)
        CommandEffect.OfSQL(model, stmts)

and private CommandEffectBuilder(model : Model) =
    // shared throughout the whole command, since parameters are too.
    let inference = TypeInferenceContext() :> ITypeInferenceContext
    let inferredStmts = ResizeArray()
    let mutable newModel = None
    member this.AddStatement(stmt : Stmt) =
        let model = newModel |? model
        let checker = TypeChecker(inference, InferredSelectScope.Root(model))
        let inferredStmt = checker.Stmt(stmt)
        inferredStmts.Add(inferredStmt)
        newModel <- ModelChange(model, inference).Statement(inferredStmt)
    member this.CommandEffect() =
        let mapping = concreteMapping inference
        let stmts = inferredStmts |> Seq.map mapping.Stmt |> toReadOnlyList
        let pars =
            inference.Parameters
            |> Seq.map (fun p -> p, inference.Concrete(inference.Variable(p)))
            |> toReadOnlyList
        let references = lazy ReadWriteReferences.references stmts
        {   Statements = stmts
            ModelChange = newModel
            Parameters = pars
            WriteTables =
                seq {
                    for ref in references.Value.TablesWritten -> ref.SchemaName, ref.TableName
                }
            ReadTables =
                seq {
                    for ref in references.Value.TablesRead -> ref.SchemaName, ref.TableName
                }
        }