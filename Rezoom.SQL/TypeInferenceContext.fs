namespace Rezoom.SQL
open System
open System.Collections.Generic
open Rezoom.SQL
open Rezoom.SQL.InferredTypes

type private TypeInferenceContext() =
    let variablesByParameter = Dictionary<BindParameter, InferredType>()
    let variablesById = Dictionary<TypeVariableId, InferredType>()
    let mutable nextVariableId = 0
    let getVar id =
        let succ, inferred = variablesById.TryGetValue(id)
        if not succ then bug "Type variable not found"
        else inferred
    static member UnifyColumnTypes(left : ColumnType, right : ColumnType) =
        result {
            let nullable = max left.Nullable right.Nullable
            let! ty = left.Type.Unify(right.Type)
            return { Nullable = nullable; Type = ty }
        }
    member this.AnonymousVariable() =
        let id = nextVariableId
        let var = TypeVariable id
        nextVariableId <- nextVariableId + 1
        variablesById.Add(id, InferredType.Any)
        var
    member this.Variable(bindParameter) =
        let succ, v = variablesByParameter.TryGetValue(bindParameter)
        if succ then v else
        let id = nextVariableId
        let var = TypeVariable id
        nextVariableId <- nextVariableId + 1
        variablesByParameter.Add(bindParameter, var)
        variablesById.Add(id, InferredType.Any)
        var
    member this.Unify(source, left, right) =
        match left, right with
        | ConcreteType left, ConcreteType right ->
            let col = TypeInferenceContext.UnifyColumnTypes(left, right) |> resultAt source
            ConcreteType col
        | TypeVariable left, right ->
            let var = getVar left
            let unified = this.Unify(source, var, right)
            variablesById.[left] <- unified
            unified
        | left, TypeVariable right ->
            let var = getVar right
            let unified = this.Unify(source, left, var)
            variablesById.[right] <- unified
            unified
        | DependentlyNullType (onType, colType), right ->
            // pretend like it's not null for unification purposes
            let colType = this.Unify(source, colType, right)
            DependentlyNullType(onType, colType)
        | left, DependentlyNullType (onType, colType) ->
            let colType = this.Unify(source, left, colType)
            DependentlyNullType(onType, colType)
    member this.Concrete(inferred) =
        match inferred with
        | ConcreteType concrete -> concrete
        | TypeVariable id ->
            this.Concrete(getVar id)
        | DependentlyNullType (onType, colType) ->
            let ifType = this.Concrete(onType)
            let thenType = this.Concrete(colType)
            { thenType with Nullable = ifType.Nullable }
    interface ITypeInferenceContext with
        member this.AnonymousVariable() = this.AnonymousVariable()
        member this.Variable(parameter) = this.Variable(parameter)
        member this.Unify(source, left, right) = this.Unify(source, left, right)
        member this.Concrete(inferred) = this.Concrete(inferred)
        member __.Parameters = variablesByParameter.Keys :> _ seq
 
[<AutoOpen>]
module private TypeInferenceExtensions =
    type ITypeInferenceContext with
        member typeInference.Unify(source : SourceInfo, inferredType, coreType : CoreColumnType) =
            typeInference.Unify(source, inferredType, InferredType.Dependent(inferredType, coreType))
        member typeInference.Unify(source : SourceInfo, types : InferredType seq) =
            types
            |> Seq.fold
                (fun s next -> typeInference.Unify(source, s, next))
                InferredType.Any
        member typeInference.Concrete(inferred) = typeInference.Concrete(inferred)
        member typeInference.Binary(source, op, left, right) =
            match op with
            | Concatenate -> typeInference.Unify(source, [ left; right; InferredType.String ])
            | Multiply
            | Divide
            | Add
            | Subtract -> typeInference.Unify(source, [ left; right; InferredType.Number ])
            | Modulo
            | BitShiftLeft
            | BitShiftRight
            | BitAnd
            | BitOr -> typeInference.Unify(source, [ left; right; InferredType.Integer ])
            | LessThan
            | LessThanOrEqual
            | GreaterThan
            | GreaterThanOrEqual
            | Equal
            | NotEqual
            | Is
            | IsNot ->
                let operandType = typeInference.Unify(source, left, right)
                InferredType.Dependent(operandType, BooleanType)
            | And
            | Or -> typeInference.Unify(source, [ left; right; InferredType.Boolean ])
        member typeInference.Unary(source, op, operandType) =
            match op with
            | Negative
            | BitNot -> typeInference.Unify(source, operandType, InferredType.Number)
            | Not -> typeInference.Unify(source, operandType, InferredType.Boolean)
            | IsNull
            | NotNull -> InferredType.Boolean
        member typeInference.AnonymousQueryInfo(columnNames) =
            {   Columns =
                    seq {
                        for { WithSource.Source = source; Value = name } in columnNames ->
                            {   ColumnName = name
                                FromAlias = None
                                Expr =
                                    {   Value = ColumnNameExpr { Table = None; ColumnName = name }
                                        Source = source
                                        Info = ExprInfo.OfType(typeInference.AnonymousVariable())
                                    }
                            }
                    } |> toReadOnlyList
            }

    let inline implicitAlias column =
        match column with
        | _, (Some _ as a) -> a
        | ColumnNameExpr c, None -> Some c.ColumnName
        | _ -> None

