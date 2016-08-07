[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rezoom.ORM.Blueprint
open LicenseToCIL
open System
open System.Collections
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open System.Text.RegularExpressions

let private blueprintCache = new Dictionary<Type, Blueprint>()

let private ciDictionary keyValues =
    let dictionary = new Dictionary<string, _>(StringComparer.OrdinalIgnoreCase)
    for key, value in keyValues do
        dictionary.[key] <- value // overwrite duplicates, last wins
    dictionary

/// Get the constructor that the blueprint for `ty` should use.
/// This is simply the constructor with the most parameters,
/// unless there is a constructor with `[<BlueprintConstructor>]`,
/// in which case that one will be used.
let private pickConstructor (ty : Type) =
    let constructors = ty.GetConstructors()
    if Array.isEmpty constructors then failwithf "Type %O has no public constructors" ty
    let constructorsWithInfo =
        constructors
        |> Array.map (fun cons ->
            let hasAttr = not << isNull <| cons.GetCustomAttribute<BlueprintConstructorAttribute>()
            cons, cons.GetParameters(), hasAttr)
    let attributed =
        constructorsWithInfo
        |> Seq.filter (fun (_, _, a) -> a)
        |> Seq.truncate 2
        |> Seq.toList
    match attributed with
    | [] ->
        constructorsWithInfo
        |> Array.maxBy (fun (_, p, _) -> p.Length)
        |> fun (cons, pars, _) -> cons, pars
    | [(cons, pars, _)] -> cons, pars
    | multiple ->
        failwithf "Type %O has %d constructors with [<BlueprintConstructor>] applied. Cannot disambiguate constructor."
            ty
            (List.length multiple)

/// Pick, in order of most to least preferred:
/// - the column whose getter is annotated with [<DataAnnotations.Key>] 
/// - the column named "ID"
/// - the column named "{TypeName}ID"
let private pickIdentity (ty : Type) (cols : IReadOnlyDictionary<string, Column>) =
    let attributed =
        seq {
            for col in cols.Values do
                match col.Getter with
                | None -> ()
                | Some getter ->
                    let attr = getter.MemberInfo.GetCustomAttribute<DataAnnotations.KeyAttribute>()
                    if not (isNull attr) then yield col
        } |> Seq.truncate 2 |> Seq.toList
    match attributed with
    | [] ->
        let succ, id = cols.TryGetValue("ID")
        if succ then Some id else
        let succ, id = cols.TryGetValue(ty.Name + "ID")
        if succ then Some id else
        None
    | [col] -> Some col
    | multiple ->
        failwithf
            "Type %O has %d columns with [<Key>] applied. Cannot disambiguate key (composite keys are not supported)."
            ty
            (List.length multiple)

let private isQueryParent (columnName : string) (setter : Setter) =
    // TODO look at getter; if it's a record property it couldn't be on the setter
    let attr =
        match setter with
        | SetConstructorParameter par -> par.GetCustomAttribute<BlueprintQueryParentAttribute>()
        | SetField field -> field.GetCustomAttribute<BlueprintQueryParentAttribute>()
        | SetProperty prop -> prop.GetCustomAttribute<BlueprintQueryParentAttribute>()
    not (isNull attr)
    || columnName.Equals("QUERYPARENT", StringComparison.OrdinalIgnoreCase)

let private swapParentChild (name : string) =
    let swapper (m : Match) =
        if m.Value.Equals("PARENT", StringComparison.OrdinalIgnoreCase) then "CHILD"
        elif m.Value.Equals("CHILD", StringComparison.OrdinalIgnoreCase) then "PARENT"
        else failwith "Impossible"
    Regex.Replace(name, "PARENT|CHILD", swapper, RegexOptions.IgnoreCase)

let private pickReverseRelationship (ty : Type) (columnName : string) (neighbor : Blueprint) =
    match neighbor.Cardinality with
    | One { Shape = Composite composite } ->
        let swapped = swapParentChild columnName
        composite.Columns.Values
            |> Seq.choose (fun manyCol ->
                if manyCol.Name.IndexOf(swapped, StringComparison.OrdinalIgnoreCase) >= 0 then
                    match manyCol.Blueprint.Value.Cardinality with
                    | Many (manyElem, _) when manyElem.Output = ty -> Some manyCol
                    | _ -> None
                else None)
            |> Seq.tryHead
    | Many ({ Shape = Composite composite }, _) ->
        composite.Columns.Values
            |> Seq.filter (fun oneCol -> composite.Output <> ty || oneCol.Name <> columnName)
            |> Seq.choose (fun oneCol ->
                match oneCol.ReverseRelationship.Value with
                | Some manyCol when
                    manyCol.Name.Equals(columnName, StringComparison.OrdinalIgnoreCase) ->
                        match oneCol.Blueprint.Value.Cardinality with
                        | One elem when elem.Output = ty -> Some oneCol
                        | _ -> None
                | _ -> None)
            |> Seq.tryHead
    | _ -> None

let rec private compositeShapeOfType ty =
    let ctor, pars = pickConstructor ty
    let props =
        ty.GetProperties() |> Array.filter (fun p -> p.CanRead && p.CanWrite)
    let fields =
        ty.GetFields()
    let gettersByName =
        seq { // order is important: we want to prefer props over fields
            for field in fields do
                yield field.Name, (field.FieldType, GetField field)
            for prop in props do
                yield prop.Name, (prop.PropertyType, GetProperty prop)
        } |> ciDictionary
    let settersByName =
        seq { // order is important: we want to prefer constructor pars over props over fields
            for field in fields do
                yield field.Name, (field.FieldType, SetField field)
            for prop in props do
                yield prop.Name, (prop.PropertyType, SetProperty prop)
            for par in pars do
                yield par.Name, (par.ParameterType, SetConstructorParameter par)
        } |> ciDictionary
    let columns = 
        seq {
            for index, KeyValue(name, (setterTy, setter)) in settersByName |> Seq.indexed ->
                let succ, getter = gettersByName.TryGetValue(name)
                let getter = 
                    if not succ then None else
                    let getterTy, getter = getter
                    if getterTy.IsAssignableFrom(setterTy) then Some getter
                    else None
                let blueprint = lazy ofType setterTy
                name, {
                    ColumnId = index
                    Name = name
                    Blueprint = blueprint
                    Setter = setter
                    Getter = getter
                    ReverseRelationship =
                        lazy pickReverseRelationship ty name blueprint.Value
                }
        } |> List.ofSeq |> ciDictionary
    {   Output = ty
        Constructor = ctor
        Identity = pickIdentity ty columns
        Columns = columns
    }

and private cardinalityOfType (ty : Type) =
    let ty = CollectionConverters.representativeForInterface ty
    let ifaces = ty.GetInterfaces()
    // For this to be a collection, it must implement IEnumerable.
    if ifaces |> Array.contains (typeof<IEnumerable>) |> not then One (elementOfType ty) else
    // Ok, really it needs to be a generic IEnumerable *of* something...
    let possible =
        ifaces
        |> Seq.filter
            (fun iface ->
                iface.IsConstructedGenericType
                && iface.GetGenericTypeDefinition() = typedefof<_ seq>)
        |> Seq.truncate 2
        |> Seq.toList
    match possible with
    | [] -> One (elementOfType ty)
    | [ienum] ->
        // Also, we need to figure out some way to construct it.
        let elemTy =
            match ienum.GetGenericArguments() with
            | [|e|] -> e
            | _ -> failwith "Cannot run in bizzare universe where IEnumerable<T> doesn't have one generic arg."
        match CollectionConverters.converter ty ienum elemTy with
        | None -> One (elementOfType ty)
        | Some converter -> Many (elementOfType elemTy, converter)
    | multiple ->
        failwithf "Type %O has %d IEnumerable<T> implementations. This confuses us."
            ty
            (List.length multiple)

and private primitiveShapeOfType (ty : Type) =
    PrimitiveConverters.converter ty
    |> Option.map (fun converter -> { Output = ty; Converter = converter })

and private elementOfType (ty : Type) =
    let shape =
        match primitiveShapeOfType ty with
        | Some p -> Primitive p
        | None -> Composite (compositeShapeOfType ty)
    {
        Shape = shape
        Output = ty
    }

and private ofTypeRaw (ty : Type) =
    match primitiveShapeOfType ty with
    | Some p ->
        {
            Cardinality =
                {
                    Shape = Primitive p
                    Output = ty
                } |> One
            Output = ty
        }
    | None -> 
        {
            Cardinality = cardinalityOfType ty 
            Output = ty
        }

and ofType ty =
    lock blueprintCache <| fun () ->
    let succ, existing = blueprintCache.TryGetValue(ty)
    if succ then existing else
    let blueprint = ofTypeRaw ty
    blueprintCache.[ty] <- blueprint
    blueprint
