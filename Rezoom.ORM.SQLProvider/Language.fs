namespace Rezoom.ORM.SQLProvider
open System
open System.Collections.Generic

type InferredTypeCore =
    | AnyType
    | OneOfTypes of CoreColumnType Set
    | ExactlyType of CoreColumnType
    static member (=^=) (left : InferredTypeCore, right : InferredTypeCore) =
        match left, right with
        | AnyType, y -> Some y
        | x, AnyType -> Some x
        | OneOfTypes xset, ExactlyType y ->
            if xset |> Set.contains y then Some right
            else None
        | OneOfTypes xset, OneOfTypes yset ->
            let intersection = Set.intersect xset yset
            match Set.count intersection with
            | 0 -> None
            | 1 -> Set.toList intersection |> List.head |> ExactlyType |> Some
            | _ -> intersection |> OneOfTypes |> Some
        | y, x -> x =^= y

type InferredType =
    {
        Inferred : InferredTypeCore
        Nullable : bool option
    }
    static member Boolean = { Nullable = None; Inferred = ExactlyType BooleanType }
    static member String = { Nullable = None; Inferred = ExactlyType StringType }
    static member Any = { Nullable = None; Inferred = AnyType }
    static member (=^=) (left, right) =
        let inferred = left.Inferred =^= right.Inferred
        match inferred with
        | None -> None
        | Some intersection ->
            {
                Inferred = intersection
                Nullable =
                    match left.Nullable, right.Nullable with
                    | None, Some y -> Some y
                    | Some x, None -> Some x
                    | None, None -> None
                    | Some x, Some y -> Some (max x y)
            } |> Some

type Parameter =
    abstract member ParameterName : string
    abstract member InferredType : InferredType

type ILanguageStatement =
    abstract member ModelChange : IModel option
    abstract member ResultSets : ISchemaQuery IReadOnlyList
    abstract member TablesRead : ISchemaTable IReadOnlyList
    abstract member TablesWritten : ISchemaTable IReadOnlyList
    abstract member Parameters : Parameter IReadOnlyList