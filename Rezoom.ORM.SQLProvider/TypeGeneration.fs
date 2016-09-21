module Rezoom.ORM.SQLProvider.TypeGeneration
open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open System.Reflection
open FSharp.Core.CompilerServices
open FSharp.Quotations
open FSharp.Reflection
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

let private parameterIndexer (pars : BindParameter seq) =
    let dict =
        pars |> Seq.indexed |> Seq.map (fun (a, b) -> (b, a)) |> dict
    { new IParameterIndexer with
        member __.ParameterIndex(par) = dict.[par]
    }

let private localNameCase, private commandTextCase, private parameterCase =
    let cases = FSharpType.GetUnionCases(typeof<CommandFragment>)
    ( cases |> Array.find (fun c -> c.Name = "LocalName")
    , cases |> Array.find (fun c -> c.Name = "CommandText")
    , cases |> Array.find (fun c -> c.Name = "Parameter")
    )

let private toFragmentExpr (fragment : CommandFragment) =
    match fragment with
    | LocalName n -> Expr.NewUnionCase(localNameCase, [ Quotations.Expr.Value(n) ])
    | CommandText t -> Expr.NewUnionCase(commandTextCase, [ Quotations.Expr.Value(t) ])
    | Parameter i -> Expr.NewUnionCase(parameterCase, [ Quotations.Expr.Value(i) ])

let private toFragmentArrayExpr (fragments : CommandFragment IReadOnlyList) =
    Expr.NewArray(typeof<CommandFragment>, fragments |> Seq.map toFragmentExpr |> Seq.toList)

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
        let subTy = generateRowTypeFromColumns model (name + "Row") subMap
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

let private generateRowType (model : UserModel) (name : string) (query : SchemaQuery) =
    CompileTimeColumnMap.Parse(query.Columns)
    |> generateRowTypeFromColumns model name

let private generateCtor (baseCtor : ConstructorInfo) (generate : GenerateType) =
    let backend = generate.UserModel.Backend
    let parameters = generate.Command.Effect.Parameters
    let indexer = parameterIndexer (parameters |> Seq.map fst)
    let fragments = backend.ToCommandFragments(indexer, generate.Command.Statements)
    let parameters =
        [ for NamedParameter name, ty in parameters ->
            ProvidedParameter(name.Value, generate.UserModel.Backend.MapPrimitiveType(ty))
        ]
    let ctor = ProvidedConstructor(parameters)
    ctor.BaseConstructorCall <-
        function
        | this :: args ->
            let arr = Expr.NewArray(typeof<obj>, args |> List.map (fun e -> Expr.Coerce(e, typeof<obj>)))
            let frags = toFragmentArrayExpr fragments
            baseCtor, [ frags; arr ]
        | _ ->
            failwith "Invalid base constructor call argument list"
    ctor.InvokeCode <- fun _ -> <@@ () @@>
    ctor

let generateType (generate : GenerateType) =
    let rowTypes, baseTy =
        let genRowType = generateRowType generate.UserModel
        match generate.Command.Effect.ResultSets |> Seq.toList with
        | [] -> [], typeof<Rezoom.ORM.Command0<unit>>
        | [ resultSet ] ->
            let rowType = genRowType "Row" resultSet
            [ rowType ],
                typedefof<Rezoom.ORM.Command1<_>>.MakeGenericType(rowType)
        | [ resultSet1; resultSet2 ] ->
            let rowType1 = genRowType "Row1" resultSet1
            let rowType2 = genRowType "Row2" resultSet2
            [ rowType1; rowType2 ],
                typedefof<Rezoom.ORM.Command2<_, _>>.MakeGenericType(rowType1, rowType2)
        | [ resultSet1; resultSet2; resultSet3 ] ->
            let rowType1 = genRowType "Row1" resultSet1
            let rowType2 = genRowType "Row2" resultSet2
            let rowType3 = genRowType "Row3" resultSet3
            [ rowType1; rowType2; rowType3 ],
                typedefof<Rezoom.ORM.Command3<_, _, _>>.MakeGenericType(rowType1, rowType2, rowType3)
        | _ ->
            failwithf "Too many (%d) result sets from command." generate.Command.Effect.ResultSets.Count
    let provided =
        ProvidedTypeDefinition(generate.Assembly, generate.Namespace, generate.TypeName, Some baseTy, IsErased = false)
    provided.AddMembers rowTypes
    let baseCtor = baseTy.GetConstructor([| typeof<CommandFragment IReadOnlyList>; typeof<obj IReadOnlyList> |])
    if isNull baseCtor then failwith "No matching base ctor"
    provided.AddMember <| generateCtor baseCtor generate
    provided