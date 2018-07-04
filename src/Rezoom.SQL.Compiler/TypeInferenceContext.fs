namespace Rezoom.SQL.Compiler
open System
open System.Collections.Generic
open Rezoom.SQL.Compiler.InferredTypes

type private TypeInferenceVariable(id : TypeVariableId) =
    let inferredType = TypeVariable id
    let mutable currentType = AnyTypeClass
    member __.Id = id
    member __.InferredType = inferredType
    member __.CurrentType = currentType
    member __.Unify(source, core : CoreColumnType) =
        let unified = currentType.Unify(core) |> resultAt source
        currentType <- unified

type private NullabilityVariable(id : TypeVariableId) =
    let inferredNullable = NullableVariable id
    let mutable currentNullable = NullableUnknown
    member __.Id = id
    member __.InferredNullable = inferredNullable
    member __.CurrentNullable = currentNullable
    member __.ForceNullable() =
        currentNullable <- NullableKnown true

type private VariableTracker<'var>(init : TypeVariableId -> 'var, id : 'var -> TypeVariableId) =
    let variablesByParameter = Dictionary<BindParameter, TypeVariableId>()
    let variablesById = ResizeArray<'var>()
    let getVar id =
        if id < 0 || id >= variablesById.Count then bug "Type variable not found"
        variablesById.[id]
    member this.BoundParameters = variablesByParameter.Keys
    member this.NextVar() =
        let var = init variablesById.Count
        variablesById.Add(var)
        var
    member this.GetVar(id : TypeVariableId) = getVar id
    member this.BindVar(bindParameter : BindParameter) =
        let succ, v = variablesByParameter.TryGetValue(bindParameter)
        if succ then getVar v else
        let var = this.NextVar()
        variablesByParameter.[bindParameter] <- id var
        var
    member this.Replace(id : TypeVariableId, var : 'var) =
        variablesById.[id] <- var

type private TypeInferenceContext() =
    let typeVariables = VariableTracker<TypeInferenceVariable>(TypeInferenceVariable, fun v -> v.Id)
    let nullVariables= VariableTracker<NullabilityVariable>(NullabilityVariable, fun v -> v.Id)
    let deferredNullables = ResizeArray()
    static member UnifyColumnTypes(left : ColumnType, right : ColumnType) =
        result {
            let nullable = max left.Nullable right.Nullable
            let! ty = left.Type.Unify(right.Type)
            return { Nullable = nullable; Type = ty }
        }
    member this.AnonymousVariable() = typeVariables.NextVar().InferredType
    member private this.Variable(bindParameter) =
        {   InferredType = typeVariables.BindVar(bindParameter).InferredType
            InferredNullable = nullVariables.BindVar(bindParameter).InferredNullable
        }
    member this.Unify(source, left, right) =
        match left, right with
        | TypeKnown lk, TypeKnown rk ->
            lk.Unify(rk) |> resultAt source |> TypeKnown
        | TypeVariable varId, TypeKnown knownType
        | TypeKnown knownType, TypeVariable varId ->
            let tvar = typeVariables.GetVar(varId)
            tvar.Unify(source, knownType)
            tvar.InferredType
        | TypeVariable leftId, TypeVariable rightId ->
            let left, right = typeVariables.GetVar(leftId), typeVariables.GetVar(rightId)
            left.Unify(source, right.CurrentType)
            typeVariables.Replace(rightId, left)
            left.InferredType
    member this.UnifyList(source, elem, list) =
        let var = typeVariables.BindVar(list)
        match elem with
        | TypeVariable varId ->
            var.Unify(source, ListType (typeVariables.GetVar(varId)).CurrentType)
        | TypeKnown knownType ->
            var.Unify(source, ListType knownType)
    member this.ForceNullable(nullable : InferredNullable) =
        match nullable.Simplify() with
        | NullableDueToJoin _
        | NullableUnknown
        | NullableKnown _ -> () // even NullableKnown false is OK, we just want to force the NullableVariables
        | NullableVariable id -> nullVariables.GetVar(id).ForceNullable()
        | NullableEither _ ->
            let rec allVars v =
                match v with
                | NullableUnknown
                | NullableKnown true 
                | NullableKnown false
                | NullableDueToJoin _ -> Seq.empty
                | NullableVariable id -> Seq.singleton id
                | NullableEither (l, r) -> Seq.append (allVars l) (allVars r)
            deferredNullables.Add(ResizeArray(allVars nullable))
    member this.ResolveNullable(nullable) =
        if deferredNullables.Count > 0 then
            let triviallySatisfied r =
                let t = NullableKnown true
                r |> Seq.exists (fun v -> nullVariables.GetVar(v).CurrentNullable = t)
            ignore <| deferredNullables.RemoveAll(fun r -> triviallySatisfied r) // remove trivially satisfied reqs
            for vs in deferredNullables do // remaining vars must all be forced null
                for v in vs do
                    nullVariables.GetVar(v).ForceNullable()
            deferredNullables.Clear()
        match nullable with
        | NullableUnknown -> false
        | NullableDueToJoin _ -> true
        | NullableKnown t -> t
        | NullableVariable id -> this.ResolveNullable(nullVariables.GetVar(id).CurrentNullable)
        | NullableEither (l, r) -> this.ResolveNullable(l) || this.ResolveNullable(r)
    member this.Concrete(inferred) =
        {   Nullable = this.ResolveNullable(inferred.InferredNullable)
            Type =
                match inferred.InferredType with
                | TypeKnown t -> t
                | TypeVariable id -> typeVariables.GetVar(id).CurrentType
        }
    interface ITypeInferenceContext with
        member this.AnonymousVariable() = this.AnonymousVariable()
        member this.Variable(parameter) = this.Variable(parameter) 
        member this.UnifyList(source, elem, list) = this.UnifyList(source, elem, list)
        member this.Unify(source, left, right) = this.Unify(source, left, right)
        member this.ForceNullable(_ : SourceInfo, nullable) = this.ForceNullable(nullable)
        member this.Concrete(inferred) = this.Concrete(inferred)
        member __.Parameters = typeVariables.BoundParameters :> _ seq
 
[<AutoOpen>]
module private TypeInferenceExtensions =
    type ITypeInferenceContext with
        member typeInference.Unify(source : SourceInfo, left : InferredType, right : CoreColumnType) =
            { left with
                InferredType = typeInference.Unify(source, left.InferredType, TypeKnown right)
            }
        member typeInference.Unify(source : SourceInfo, left : InferredType, right : InferredType) =
            {   InferredType = typeInference.Unify(source, left.InferredType, right.InferredType)
                InferredNullable = InferredNullable.Either(left.InferredNullable, right.InferredNullable)
            }
        member typeInference.Unify(source : SourceInfo, types : CoreInferredType seq) =
            types
            |> Seq.fold
                (fun s next -> typeInference.Unify(source, s, next))
                InferredType.Scalar.InferredType
        member typeInference.Unify(source : SourceInfo, types : InferredType seq) =
            {   InferredType = typeInference.Unify(source, types |> Seq.map (fun t -> t.InferredType))
                InferredNullable = InferredNullable.Any(types |> Seq.map (fun t -> t.InferredNullable))
            }
        /// Unify a known type (e.g. from a table we're inserting into or a declared CTE)
        /// with an inferred type. The inferred type is forced nullable if the known type is nullable.
        member typeInference.UnifyLeftKnown(source : SourceInfo, left : InferredType, right : InferredType) =
            ignore <| typeInference.Unify(source, left.InferredType, right.InferredType)
            if left.InferredNullable = NullableKnown true then
                typeInference.ForceNullable(source, right.InferredNullable)
        member typeInference.Concrete(inferred) = typeInference.Concrete(inferred)
        member typeInference.Binary(source, op, left, right) =
            match op with
            | Concatenate -> typeInference.Unify(source, [ left; right; InferredType.Stringish ])
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
            | NotEqual ->
                let operandType = typeInference.Unify(source, left, right)
                InferredType.Dependent(operandType, BooleanType)
            | Is
            | IsNot ->
                let operandType = typeInference.Unify(source, left, right)
                typeInference.ForceNullable(source, left.InferredNullable)
                typeInference.ForceNullable(source, right.InferredNullable)
                InferredType.Dependent(operandType, BooleanType)
            | And
            | Or -> typeInference.Unify(source, [ left; right; InferredType.Boolean ])
        member typeInference.Unary(source, op, operandType) =
            match op with
            | Negative
            | BitNot -> typeInference.Unify(source, operandType, InferredType.Number)
            | Not -> typeInference.Unify(source, operandType, InferredType.Boolean)
        member typeInference.AnonymousQueryInfo(columnNames) =
            {   Columns =
                    seq {
                        for { WithSource.Source = source; Value = name } in columnNames ->
                            let tyVar =
                                {   InferredType = typeInference.AnonymousVariable()
                                    InferredNullable = NullableUnknown
                                } |> ExprInfo.OfType
                            {   ColumnName = name
                                FromAlias = None
                                Expr =
                                    {   Value = ColumnNameExpr { Table = None; ColumnName = name }
                                        Source = source
                                        Info = tyVar
                                    }
                            }
                    } |> toReadOnlyList
                StaticRowCount = None
                ClausesIdempotent = true
            }
        member typeInference.Function(source : SourceInfo, func : FunctionType, invoc : InfFunctionArguments) =
            let functionVars = Dictionary()
            let aggregate = func.Aggregate(invoc)
            let term (termType : FunctionTermType) =
                match termType.TypeVariable with
                | None -> TypeKnown termType.TypeConstraint
                | Some name ->
                    let succ, tvar = functionVars.TryGetValue(name)
                    let tvar =
                        if succ then tvar else
                        let avar = typeInference.AnonymousVariable()
                        functionVars.[name] <- avar
                        avar
                    typeInference.Unify(source, tvar, TypeKnown termType.TypeConstraint)
            match invoc with
            | ArgumentWildcard ->
                match aggregate with
                | Some aggregate when aggregate.AllowWildcard ->
                    ArgumentWildcard,
                        {   InferredType = term func.Returns
                            InferredNullable =
                                if func.Returns.ForceNullable then NullableKnown true else NullableUnknown
                        }
                | _ -> failAt source <| Error.functionDoesNotPermitWildcard func.FunctionName
            | ArgumentList (distinct, args) as argumentList ->
                if Option.isSome distinct then
                    match aggregate with
                    | Some aggregate when aggregate.AllowDistinct -> ()
                    | _ -> failAt source <| Error.functionDoesNotPermitDistinct func.FunctionName
                let nulls = ResizeArray()
                func.ValidateArgs(source, args, (fun a -> a.Source), fun arg termTy ->
                    let term = term termTy
                    ignore <| typeInference.Unify(arg.Source, arg.Info.Type.InferredType, term)
                    if termTy.ForceNullable then
                        typeInference.ForceNullable(arg.Source, arg.Info.Type.InferredNullable)
                    if termTy.InfectNullable then
                        nulls.Add(arg.Info.Type.InferredNullable))
                let returnType =
                    {   InferredType = term func.Returns
                        InferredNullable =
                            if func.Returns.ForceNullable then NullableKnown true else InferredNullable.Any(nulls)
                    }
                argumentList, returnType

    let inline implicitAlias column =
        match column with
        | _, (Some _ as a) -> a
        | ColumnNameExpr c, None -> Some c.ColumnName
        | _ -> None

