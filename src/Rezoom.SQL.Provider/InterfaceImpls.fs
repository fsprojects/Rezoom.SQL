module private Rezoom.SQL.Provider.InterfaceImpls
open System
open System.Reflection
open Rezoom.SQL.Compiler
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

let implement (ty : ProvidedTypeDefinition) (props : ProvidedProperty ResizeArray) (interfaceTy : Type) (location : SourceInfo) =
    let propsByName = props |> Seq.map (fun p -> p.Name, p) |> Map.ofSeq
    ty.AddInterfaceImplementation interfaceTy
    for iProp in interfaceTy.GetProperties(BindingFlags.Public|||BindingFlags.Instance) do
        if iProp.CanWrite then
            failAt location <| Error.rowTypeMayOnlyImplementReadOnlyInterface interfaceTy.FullName
        match propsByName |> Map.tryFind iProp.Name with
        | None ->
            failAt location <| Error.rowTypeIsMissingInterfaceProperty interfaceTy.FullName iProp.Name
        | Some found ->
            let actualType = found.PropertyType.FullName
            let iPropType = iProp.PropertyType.FullName
            if actualType <> iPropType then
                failAt location <|
                    Error.rowPropertyHasWrongTypeForInterfaceProperty
                        found.Name
                        actualType
                        interfaceTy.FullName
                        iPropType
            let getterMethod =
                ProvidedMethod(interfaceTy.Name + "." + iProp.Name, [], iProp.PropertyType, invokeCode =
                    function
                    | [ this ] -> Expr.Call(this, found.GetMethod, [])
                    | _ -> bug "Invalid getter argument list")
            let flags =
                MethodAttributes.Virtual
                ||| MethodAttributes.Private
                ||| MethodAttributes.Final
                ||| MethodAttributes.NewSlot
                ||| MethodAttributes.HasSecurity
            getterMethod.SetMethodAttrs(flags)
            let ifaceMethod = iProp.GetMethod
            ty.DefineMethodOverride(getterMethod, ifaceMethod)
