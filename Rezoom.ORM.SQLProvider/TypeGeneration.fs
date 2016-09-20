module Rezoom.ORM.SQLProvider.TypeGeneration
open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open Rezoom.ORM
open SQLow
open SQLow.CommandEffect

type GenerateType =
    {
        UserModel : UserModel
        Assembly : Assembly
        Namespace : string
        TypeName : string
        Command : CommandWithEffect
    }

/// Lowercase initial uppercase characters.
let private toCamelCase (str : string) =
    Regex.Replace(str, @"^\p{Lu}+", fun m -> m.Value.ToLowerInvariant())

type private KeyType() =
    [<ComponentModel.DataAnnotations.Key>]
    member __.DummyKeyProperty = 0
    static member AttributeData =
        CustomAttributeData.GetCustomAttributes(typeof<KeyType>.GetProperty("DummyKey"))
        |> Seq.find (fun a -> a.AttributeType = typeof<ComponentModel.DataAnnotations.KeyAttribute>)

let rec private generateRowTypeFromColumns (model : UserModel) name (columnMap : CompileTimeColumnMap) =
    let ty =
        ProvidedTypeDefinition
            ( name
            , Some typeof<obj>
            , IsErased = false
            )
    let fields = ResizeArray()
    let addField pk name fieldTy =
        let camel = toCamelCase name
        let field = ProvidedField("_" + camel, fieldTy)
        field.SetFieldAttributes(FieldAttributes.Private)
        let getter = ProvidedProperty(name, fieldTy)
        if pk then
            getter.AddCustomAttribute(KeyType.AttributeData)
        getter.GetterCode <-
            function
            | [ this ] -> Expr.FieldGet(this, field)
            | _ -> failwith "Invalid getter argument list"
        ty.AddMembers [ field :> MemberInfo; getter :> _ ]
        fields.Add(camel, field)
    for KeyValue(name, (_, column)) in columnMap.Columns do
        addField column.PrimaryKey name <| model.Backend.MapPrimitiveType(column.ColumnType)
    for KeyValue(name, subMap) in columnMap.SubMaps do
        let subTy = generateRowTypeFromColumns model name subMap
        ty.AddMember(subTy)
        addField false name subTy
    let ctorParams = [ for camel, field in fields -> ProvidedParameter(camel, field.FieldType) ]
    let ctor = ProvidedConstructor(ctorParams)
    ctor.InvokeCode <-
        function
        | this :: pars ->
            Seq.zip fields pars
            |> Seq.fold
                (fun exp ((_, field), par) -> Expr.Sequential(exp, Expr.FieldSet(this, field, par)))
                (Quotations.Expr.Value(()))
        | _ -> failwith "Invalid ctor argument list"
    ty.AddMember(ctor)
    ty

let private generateRowType (name : string) (model : UserModel) (query : SchemaQuery) =
    CompileTimeColumnMap.Parse(query.Columns)
    |> generateRowTypeFromColumns model name

let generateType generate =
    let baseType = typeof<obj> // TODO pick base command type based on result sets
    let genTy =
        ProvidedTypeDefinition
            ( generate.Assembly
            , generate.Namespace
            , generate.TypeName
            , Some baseType
            , IsErased = false
            )
    genTy