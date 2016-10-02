namespace StaticQL
open System
open System.Collections.Generic
open StaticQL.InferredTypes

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
            | x, y ->
                Error <| sprintf "The types %A and %A cannot be unified" x y
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
                        sprintf "There is no intersection between the types %A and %A"
                            (leftTypes |> Set.toList) (rightTypes |> Set.toList)
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
                if left |> List.exists (fun t -> t.Type = right.Type) then
                    return concrete
                else
                    return! Error <|
                        sprintf "The type %A is not one of %A"
                            right.Type (left |> List.map (fun c -> c.Type) |> List.distinct)
            | (ConcreteType _ as left), (OneOfTypes _ as right) -> return! this.Unify(right, left)
        }
    member this.Concrete(inferred) =
        match inferred with
        | ConcreteType concrete -> concrete
        | OneOfTypes possible ->
            let preference = function
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
            let nullable = possible |> Seq.map (fun t -> t.Nullable) |> Seq.max
            let ty = possible |> Seq.map (fun t -> t.Type) |> Seq.maxBy preference
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
            



