module private Rezoom.ORM.CollectionConverters
open System
open System.Collections.Generic

let private interfaceRepresentatives : IDictionary<Type, Type -> Type> =
    [|
        [|
            typedefof<IEnumerable<_>>
            typedefof<IReadOnlyCollection<_>>
            typedefof<IReadOnlyList<_>>
        |], fun (elementTy : Type) -> elementTy.MakeArrayType()
        [|
            typedefof<ICollection<_>>
            typedefof<IList<_>>
        |], fun (elementTy : Type) -> typedefof<_ ResizeArray>.MakeGenericType(elementTy)
    |]
    |> Seq.collect
        (fun (ifaces, representative) -> ifaces |> Seq.map (fun i -> i, representative))
    |> dict

let representativeForInterface (ty : Type) =
    if not ty.IsInterface then ty else
    if not ty.IsConstructedGenericType then ty else
    let def = ty.GetGenericTypeDefinition()
    let args = ty.GetGenericArguments()
    match args with
    | [|elemTy|] ->
        let succ, repr = interfaceRepresentatives.TryGetValue(def)
        if not succ then ty
        else repr elemTy
    | _ -> ty

let converter (ty : Type) (ienum : Type) (elem : Type) : ConversionMethod option =
    None