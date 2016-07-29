[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rezoom.ORM.Blueprint
open Rezoom.ADO.Materialization
open System
open System.Reflection

let staticConversionMethod (meth : MethodInfo) =
    fun (il : Emit.ILGenerator) -> 
        il.Emit(Emit.OpCodes.Call, meth)

let rec shapeOfType (ty : Type) =
    if PrimitiveConverter.IsPrimitive(ty) then
        {
            Converter = staticConversionMethod (PrimitiveConverter.ToType(ty))
        } |> Primitive
    else
        
and 

let rec ofType (ty : Type) =
    {
        Cardinality = 
        Output = ty
    }