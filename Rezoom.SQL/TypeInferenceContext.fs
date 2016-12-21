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
        let nullable = max left.Nullable right.Nullable
        let ty =
            match left.Type, right.Type with
            | x, y when x = y -> Ok x
            | AnyType, y -> Ok y
            | x, AnyType -> Ok x
            | IntegerType s1, IntegerType s2 ->
                Ok <| IntegerType (max s1 s2) // widen size
            | FloatType s1, FloatType s2 ->
                Ok <| FloatType (max s1 s2) // widen size
            | x, y ->
                Error <| sprintf "The types %O and %O cannot be unified" x y
        match ty with
        | Error e -> Error e
        | Ok ty ->
            {
                Nullable = nullable
                Type = ty
            } |> Ok
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
        | OneOfTypes left, OneOfTypes right ->
            let leftTypes = left |> Seq.map (fun t -> t.Type) |> Set.ofSeq
            let rightTypes = right |> Seq.map (fun t -> t.Type) |> Set.ofSeq
            let possibleNulls =
                let leftNulls = left |> Seq.map (fun t -> t.Nullable)
                let rightNulls = right |> Seq.map (fun t -> t.Nullable)
                Seq.append leftNulls rightNulls |> Seq.distinct
            if Set.contains AnyType leftTypes then OneOfTypes right
            elif Set.contains AnyType rightTypes then OneOfTypes left
            else
                let possibleTypes = Set.intersect leftTypes rightTypes
                if Set.isEmpty possibleTypes then
                    failAt source <|
                        sprintf "There is no intersection between the types (%s) and (%s)"
                            (leftTypes |> Seq.map string |> String.concat " | ")
                            (rightTypes |> Seq.map string |> String.concat " | ")
                else
                    [ for nullable in possibleNulls do
                        for ty in possibleTypes do
                            yield { Nullable = nullable; Type = ty }
                    ] |> OneOfTypes
        | (OneOfTypes _ as left), (ConcreteType right) ->
            this.Unify(source, left, OneOfTypes [ right ])
        | (ConcreteType left), (OneOfTypes _ as right) ->
            this.Unify(source, OneOfTypes [ left ], right)

    member private this.Preference(ty) =
        // if given a choice between many types, we prefer to assume the one with the highest score here
        match ty with
        | AnyType -> -1
        | BinaryType -> 0
        | BooleanType -> 1
        | IntegerType Integer8 -> 2
        | IntegerType Integer16 -> 3
        | IntegerType Integer32 -> 4
        | IntegerType Integer64 -> 5
        | FloatType Float32 -> 6
        | FloatType Float64 -> 7
        | DecimalType -> 8
        | StringType -> 9
        | DateTimeType -> 10
        | DateTimeOffsetType -> 11
    member this.Concrete(inferred) =
        match inferred with
        | ConcreteType concrete -> concrete
        | OneOfTypes possible ->
            let nullable = possible |> Seq.map (fun t -> t.Nullable) |> Seq.max
            let ty = possible |> Seq.map (fun t -> t.Type) |> Seq.maxBy this.Preference
            {
                Nullable = nullable
                Type = ty
            }
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

