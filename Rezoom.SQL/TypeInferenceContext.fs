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
    member this.Unify(left, right) =
        result {
            match left, right with
            | ConcreteType left, ConcreteType right ->
                let! col = TypeInferenceContext.UnifyColumnTypes(left, right)
                return ConcreteType col
            | TypeVariable left, right ->
                let var = getVar left
                let! unified = this.Unify(var, right)
                variablesById.[left] <- unified
                return unified
            | left, TypeVariable right ->
                let var = getVar right
                let! unified = this.Unify(left, var)
                variablesById.[right] <- unified
                return unified
            | DependentlyNullType (onType, colType), right ->
                // pretend like it's not null for unification purposes
                let! colType = this.Unify(colType, right)
                return DependentlyNullType(onType, colType)
            | left, DependentlyNullType (onType, colType) ->
                let! colType = this.Unify(left, colType)
                return DependentlyNullType(onType, colType)
            | OneOfTypes left, OneOfTypes right ->
                let leftTypes = left |> Seq.map (fun t -> t.Type) |> Set.ofSeq
                let rightTypes = right |> Seq.map (fun t -> t.Type) |> Set.ofSeq
                let possibleTypes = Set.intersect leftTypes rightTypes
                if Set.isEmpty possibleTypes then
                    return! Error <|
                        sprintf "There is no intersection between the types (%s) and (%s)"
                            (leftTypes |> Seq.map string |> String.concat " | ")
                            (rightTypes |> Seq.map string |> String.concat " | ")
                else
                    let leftNulls = left |> Seq.map (fun t -> t.Nullable)
                    let rightNulls = right |> Seq.map (fun t -> t.Nullable)
                    let possibleNulls = Seq.append leftNulls rightNulls |> Seq.distinct
                    return
                        [ for nullable in possibleNulls do
                            for ty in possibleTypes do
                                yield { Nullable = nullable; Type = ty }
                        ] |> OneOfTypes
            | OneOfTypes left, (ConcreteType right as concrete) ->
                let unified =
                    left
                    |> Seq.sortByDescending (fun t -> this.Preference t.Type)
                    |> Seq.map (fun t -> this.Unify(ConcreteType t, concrete))
                    |> Seq.tryPick (function | Error _ -> None | Ok t -> Some t)
                match unified with
                | Some unified -> return unified
                | None ->
                    return! Error <|
                        sprintf "The type %O is not one of (%s)"
                            right.Type (left |> Seq.map (fun c -> string c.Type) |> Seq.distinct |> String.concat " | ")
            | (ConcreteType _ as left), (OneOfTypes _ as right) -> return! this.Unify(right, left)
        }
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
        member this.Unify(left, right) = this.Unify(left, right)
        member this.Concrete(inferred) = this.Concrete(inferred)
        member __.Parameters = variablesByParameter.Keys :> _ seq
 
[<AutoOpen>]
module private TypeInferenceExtensions =
    type ITypeInferenceContext with
        member typeInference.Unify(inferredType, coreType : CoreColumnType) =
            typeInference.Unify(inferredType, InferredType.Dependent(inferredType, coreType))
        member typeInference.Unify(inferredType, resultType : Result<InferredType, string>) =
            match resultType with
            | Ok t -> typeInference.Unify(inferredType, t)
            | Error _ as e -> e
        member typeInference.Unify(types : InferredType seq) =
            types
            |> Seq.fold
                (function | Ok s -> (fun t -> typeInference.Unify(s, t)) | Error _ as e -> (fun _ -> e))
                (Ok InferredType.Any)
        member typeInference.Concrete(inferred) = typeInference.Concrete(inferred)
        member typeInference.Binary(op, left, right) =
            match op with
            | Concatenate -> typeInference.Unify([ left; right; InferredType.String ])
            | Multiply
            | Divide
            | Add
            | Subtract -> typeInference.Unify([ left; right; InferredType.Number ])
            | Modulo
            | BitShiftLeft
            | BitShiftRight
            | BitAnd
            | BitOr -> typeInference.Unify([ left; right; InferredType.Integer ])
            | LessThan
            | LessThanOrEqual
            | GreaterThan
            | GreaterThanOrEqual
            | Equal
            | NotEqual
            | Is
            | IsNot ->
                result {
                    let! operandType = typeInference.Unify(left, right)
                    return InferredType.Dependent(operandType, BooleanType)
                }
            | And
            | Or -> typeInference.Unify([ left; right; InferredType.Boolean ])
        member typeInference.Unary(op, operandType) =
            match op with
            | Negative
            | BitNot -> typeInference.Unify(operandType, InferredType.Number)
            | Not -> typeInference.Unify(operandType, InferredType.Boolean)
            | IsNull
            | NotNull -> result { return InferredType.Boolean }
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

