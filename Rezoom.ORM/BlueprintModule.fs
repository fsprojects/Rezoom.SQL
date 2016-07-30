[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rezoom.ORM.Blueprint
open Rezoom.ADO.Materialization
open System
open System.Collections.Generic
open System.Reflection

let private ciDictionary keyValues =
    let dictionary = new Dictionary<string, _>(StringComparer.OrdinalIgnoreCase)
    for key, value in keyValues do
        dictionary.[key] <- value // overwrite duplicates, last wins
    dictionary

let private staticConversionMethod (meth : MethodInfo) =
    fun (il : Emit.ILGenerator) -> 
        il.Emit(Emit.OpCodes.Call, meth)

let rec private shapeOfType (ty : Type) =
    if PrimitiveConverter.IsPrimitive(ty) then
        {
            Converter = staticConversionMethod (PrimitiveConverter.ToType(ty))
        } |> Primitive
    else
        compositeShapeOfType ty |> Composite
and private compositeShapeOfType ty =
    let ctor, pars = // pick most detailed constructor
        let constructors = ty.GetConstructors()
        if Array.isEmpty constructors then
            failwithf "Type %O has no public constructors" ty
        constructors
        |> Seq.map (fun c -> c, c.GetParameters())
        |> Seq.maxBy (fun (_, pars) -> pars.Length)
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
        [| for KeyValue(name, (setterTy, setter)) in settersByName ->
            let succ, (getterTy, getter) = gettersByName.TryGetValue(name)
            {
                Name = name
                Blueprint = ofType setterTy
                Setter = setter
                Getter =
                    if succ && getterTy.IsAssignableFrom(setterTy) then Some getter
                    else None
            }
        |]
    {   Constructor = ctor
        Identity = None // TODO
        Columns = columns
    }

and ofType (ty : Type) =
    {
        Element =
            {
                Shape = shapeOfType ty
                Output = ty
            }
        Cardinality = One // TODO
        Output = ty
    }