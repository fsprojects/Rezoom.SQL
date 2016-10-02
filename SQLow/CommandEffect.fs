// A command is a series of SQL statements.
// This module analyzes the effects of commands, including the tables they update, the changes they make to the model,
// and the result sets they output.
namespace SQLow
open System
open System.Collections.Generic
open SQLow.InferredTypes

type CommandEffect =
    {   Statements : TStmt IReadOnlyList
        Parameters : (BindParameter * ColumnType) IReadOnlyList
        ModelChange : Model option
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
        newModel <- ModelChange(model, inference).Statment(inferredStmt)
    member this.CommandEffect() =
        let mapping =
            ASTMapping<InferredType ObjectInfo, InferredType ExprInfo, _, _>
                ((fun t -> t.Map(inference.Concrete)), fun e -> e.Map(inference.Concrete))
        let stmts = inferredStmts |> Seq.map mapping.Stmt |> toReadOnlyList
        let pars =
            inference.Parameters
            |> Seq.map (fun p -> p, inference.Concrete(inference.Variable(p)))
            |> toReadOnlyList
        {   Statements = stmts
            ModelChange = newModel
            Parameters = pars
        }