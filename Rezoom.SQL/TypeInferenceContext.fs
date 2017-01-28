namespace Rezoom.SQL
open System
open System.Collections.Generic
open Rezoom.SQL
open Rezoom.SQL.InferredTypes

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

type private TypeInferenceContext() =
    let variablesByParameter = Dictionary<BindParameter, TypeVariableId>()
    let variablesById = ResizeArray<TypeInferenceVariable>()
    let nullsByParameter = Dictionary<BindParameter, TypeVariableId>()
    let nullsById = ResizeArray<NullabilityVariable>()
    let deferredNullables = ResizeArray()
    let getTypeVar id =
        if id < 0 || id >= variablesById.Count then bug "Type variable not found"
        variablesById.[id]
    let getNullVar id =
        if id < 0 || id >= nullsById.Count then bug "Nullability variable not found"
        nullsById.[id]
    let nextTypeVar() =
        let var = TypeInferenceVariable(variablesById.Count)
        variablesById.Add(var)
        var
    let nextNullVar() =
        let var = NullabilityVariable(nullsById.Count)
        nullsById.Add(var)
        var
    static member UnifyColumnTypes(left : ColumnType, right : ColumnType) =
        result {
            let nullable = max left.Nullable right.Nullable
            let! ty = left.Type.Unify(right.Type)
            return { Nullable = nullable; Type = ty }
        }
    member this.AnonymousVariable() = nextTypeVar().InferredType
    member private this.ParameterTypeVariable(bindParameter) =
        let succ, v = variablesByParameter.TryGetValue(bindParameter)
        if succ then getTypeVar v else
        let var = nextTypeVar()
        variablesByParameter.[bindParameter] <- var.Id
        var
    member private this.Variable(bindParameter) =
        let inferredNull =
            let succ, v = nullsByParameter.TryGetValue(bindParameter)
            if succ then getNullVar v else
            let var = nextNullVar()
            nullsByParameter.[bindParameter] <- var.Id
            var
        {   InferredType = this.ParameterTypeVariable(bindParameter).InferredType
            InferredNullable = inferredNull.InferredNullable
        }
    member this.Unify(source, left, right) =
        match left, right with
        | TypeKnown lk, TypeKnown rk ->
            lk.Unify(rk) |> resultAt source |> TypeKnown
        | TypeVariable varId, TypeKnown knownType
        | TypeKnown knownType, TypeVariable varId ->
            let tvar = getTypeVar varId
            tvar.Unify(source, knownType)
            tvar.InferredType
        | TypeVariable leftId, TypeVariable rightId ->
            let left, right = getTypeVar leftId, getTypeVar rightId
            left.Unify(source, right.CurrentType)
            variablesById.[rightId] <- left
            left.InferredType
    member this.UnifyList(source, elem, list) =
        let var = this.ParameterTypeVariable(list)
        match elem with
        | TypeVariable varId ->
            var.Unify(source, ListType (getTypeVar varId).CurrentType)
        | TypeKnown knownType ->
            var.Unify(source, ListType knownType)
    member this.ForceNullable(source, nullable : InferredNullable) =
        match nullable.Simplify() with
        | NullableDueToJoin _
        | NullableUnknown
        | NullableKnown true -> ()
        | NullableKnown false ->
            failAt source Error.exprMustBeNullable
        | NullableVariable id -> (getNullVar id).ForceNullable()
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
                r |> Seq.exists (fun v -> (getNullVar v).CurrentNullable = t)
            ignore <| deferredNullables.RemoveAll(fun r -> triviallySatisfied r) // remove trivially satisfied reqs
            for vs in deferredNullables do // remaining vars must all be forced null
                for v in vs do
                    (getNullVar v).ForceNullable()
            deferredNullables.Clear()
        match nullable with
        | NullableUnknown -> false
        | NullableDueToJoin _ -> true
        | NullableKnown t -> t
        | NullableVariable id -> this.ResolveNullable((getNullVar id).CurrentNullable)
        | NullableEither (l, r) -> this.ResolveNullable(l) || this.ResolveNullable(r)
    member this.Concrete(inferred) =
        {   Nullable = this.ResolveNullable(inferred.InferredNullable)
            Type =
                match inferred.InferredType with
                | TypeKnown t -> t
                | TypeVariable id -> (getTypeVar id).CurrentType
        }
    interface ITypeInferenceContext with
        member this.AnonymousVariable() = this.AnonymousVariable()
        member this.Variable(parameter) = this.Variable(parameter) 
        member this.UnifyList(source, elem, list) = this.UnifyList(source, elem, list)
        member this.Unify(source, left, right) = this.Unify(source, left, right)
        member this.ForceNullable(source, nullable) = this.ForceNullable(source, nullable)
        member this.Concrete(inferred) = this.Concrete(inferred)
        member __.Parameters = variablesByParameter.Keys :> _ seq
 
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
            | IsNull
            | NotNull ->
                typeInference.ForceNullable(source, operandType.InferredNullable)
                InferredType.Boolean
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

