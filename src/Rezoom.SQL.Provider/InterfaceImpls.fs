module private Rezoom.SQL.Provider.InterfaceImpls
open System
open System.Reflection
open Rezoom.SQL.Compiler
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

type ProvidedPropertyMeta =
    {   Prop : ProvidedProperty
        IsSubMap : bool
    }

let implementInterfaces (ty : ProvidedTypeDefinition) (props : ProvidedPropertyMeta ResizeArray) (interfaceTy : Type) (location : SourceInfo) =
    let propsByName = props |> Seq.map (fun p -> p.Prop.Name, p) |> Map.ofSeq
    ty.AddInterfaceImplementation interfaceTy
    for iProp in interfaceTy.GetProperties(BindingFlags.Public|||BindingFlags.Instance) do
        if iProp.CanWrite then
            failAt location <| Error.rowTypeMayOnlyImplementReadOnlyInterface interfaceTy.FullName
        match propsByName |> Map.tryFind iProp.Name with
        | None ->
            failAt location <| Error.rowTypeIsMissingInterfaceProperty interfaceTy.FullName iProp.Name
        | Some { Prop = found; IsSubMap = isSubMap } ->
            let actualType = found.PropertyType.FullName
            let iPropType = iProp.PropertyType.FullName
            let getterMethod =
                if actualType = iPropType then
                    found.GetMethod :?> ProvidedMethod // ProvidedProperty.GetMethod should always be a ProvidedMethod
                else
                    if isSubMap then
                        // This is expected for sub-maps. Our type will be a generated type that implements the needed interface
                        // (and if not, it'll have blown up by now) but won't literally BE the needed interface.
                        // Use an explicit interface implementation getter and coerce.
                        let explicitImplGet =
                            ProvidedMethod(interfaceTy.Name + "." + iProp.Name, [], iProp.PropertyType, invokeCode =
                                function
                                | [ this ] -> Expr.Coerce(Expr.Call(this, found.GetMethod, []), iProp.PropertyType)
                                | _ -> bug "Invalid getter argument list")
                        let flags =
                            MethodAttributes.Virtual
                            ||| MethodAttributes.Private
                            ||| MethodAttributes.Final
                            ||| MethodAttributes.NewSlot
                            ||| MethodAttributes.HasSecurity
                        explicitImplGet.SetMethodAttrs(flags)
                        explicitImplGet

                    else
                        failAt location <|
                            Error.rowPropertyHasWrongTypeForInterfaceProperty
                                found.Name
                                actualType
                                interfaceTy.FullName
                                iPropType
            let ifaceMethod = iProp.GetMethod
            ty.DefineMethodOverride(getterMethod, ifaceMethod)
            
