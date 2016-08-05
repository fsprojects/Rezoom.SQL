module Rezoom.ORM.CollectionConverters
open LicenseToCIL
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

type Converters<'elem> =
    static member ToArray(collection : 'elem EntityReader ICollection) =
        let arr = Array.zeroCreate collection.Count
        let mutable i = 0
        for reader in collection do
            arr.[i] <- reader.ToEntity()
            i <- i + 1
        arr
    static member ToResizeArray(collection : 'elem EntityReader ICollection) =
        let resizeArr = new ResizeArray<'elem>(collection.Count)
        for reader in collection do
            resizeArr.Add(reader.ToEntity())
        resizeArr
    static member ToList(collection : 'elem EntityReader ICollection) =
        collection |> Seq.map (fun r -> r.ToEntity()) |> List.ofSeq
    static member ToOptional(collection : 'elem EntityReader ICollection) =
        if collection.Count > 1 then failwith "More than one entity in ostensibly zero-or-one relationship"
        elif collection.Count = 1 then Some ((collection |> Seq.head).ToEntity())
        else None

let converter (ty : Type) (ienum : Type) (elem : Type) : ConversionMethod option =
    let converter = typedefof<Converters<_>>.MakeGenericType(elem)
    let specializedMethod =
        converter.GetMethods()
        |> Array.tryFind (fun m -> m.ReturnType = ty)
    match specializedMethod with
    | Some m -> Some (Ops.call1 m)
    | None ->
        // fall back to passing the type an IEnumerable<T>
        let constructorOfIEnum = ty.GetConstructor([|ienum|])
        if isNull constructorOfIEnum then None else
        let toArray = converter.GetMethod("ToArray")
        cil {
            yield Ops.call1 toArray
            yield Ops.newobj1 constructorOfIEnum
        } |> Some