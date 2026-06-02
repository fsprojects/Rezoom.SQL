module private Rezoom.SQL.Provider.InterfaceImpls
open System
open System.Reflection
open Rezoom.SQL.Compiler
open Rezoom.SQL.Mapping
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

type ProvidedPropertyMeta =
    {   Prop : ProvidedProperty
        IsSubMap : bool
    }

let private fsharpOption = "Microsoft.FSharp.Core.FSharpOption`1"

let private validGenerics =
    [|  "System.Collections.Generic.IEnumerable`1"
        "System.Collections.Generic.IReadOnlyCollection`1"
        "System.Collections.Generic.IReadOnlyList`1"
        fsharpOption
    |] |> Set.ofArray

let interfaceTyToImplementOnRowForProp (location : SourceInfo) (prop : PropertyInfo) : Type =
    let propTy = prop.PropertyType
    if propTy.IsGenericType then
        let genericDef = propTy.GetGenericTypeDefinition()
        if validGenerics.Contains(genericDef.FullName) then
            propTy.GetGenericArguments().[0]
        else
            failAt location <| Error.interfaceSubMapMustBeASupportedGenericType prop.Name propTy.FullName
    else
        propTy

let mapperCode (iProp : PropertyInfo) (found : ProvidedProperty) (row : Expr) =
    if iProp.PropertyType.IsGenericType && iProp.PropertyType.GetGenericTypeDefinition().FullName = fsharpOption then
        let internalType = iProp.PropertyType.GetGenericArguments().[0]
        let method =
            typeof<RuntimeUserConvert.RowTypeConverter>.GetMethod
                (nameof RuntimeUserConvert.RowTypeConverter.ToOptionalRowType, BindingFlags.Static ||| BindingFlags.Public)
        let method = method.MakeGenericMethod(internalType)
        Expr.Call(method, [Expr.Coerce(row, typeof<obj>)])
    else
        Expr.Coerce(row, iProp.PropertyType)

let implementInterface (ty : ProvidedTypeDefinition) (props : ProvidedPropertyMeta ResizeArray) (interfaceTy : Type) (location : SourceInfo) =
    if not interfaceTy.IsInterface then
        failAt location <| Error.onlyInterfacesAreSupportedForRowTypes interfaceTy.FullName
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
                        // Our type will be a generated type that implements the needed interface
                        // (and if not, it'll have blown up by now) but won't literally BE the needed interface.
                        // Use an explicit interface implementation getter and coerce or option-map.
                        let converter = mapperCode iProp found
                        let explicitImplGet =
                            ProvidedMethod(interfaceTy.Name + "." + iProp.Name, [], iProp.PropertyType, invokeCode =
                                function
                                | [ this ] -> converter(Expr.Call(this, found.GetMethod, []))
                                | _ -> bug "Invalid getter argument list")
                        let flags =
                            MethodAttributes.Virtual
                            ||| MethodAttributes.Private
                            ||| MethodAttributes.Final
                            ||| MethodAttributes.NewSlot
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
            
