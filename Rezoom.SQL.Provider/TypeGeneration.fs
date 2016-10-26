module Rezoom.SQL.Provider.TypeGeneration
open System
open System.Data
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open System.Reflection
open FSharp.Core.CompilerServices
open FSharp.Quotations
open FSharp.Reflection
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypes.UncheckedQuotations
open Rezoom.SQL.Mapping
open Rezoom.SQL

type GenerateTypeCase =
    | GenerateSQL of string
    | GenerateModel

type GenerateType =
    {
        UserModel : UserModel
        Assembly : Assembly
        Namespace : string
        TypeName : string
        Case : GenerateTypeCase
    }

let private parameterIndexer (pars : BindParameter seq) =
    let dict =
        pars |> Seq.indexed |> Seq.map (fun (a, b) -> (b, a)) |> dict
    { new IParameterIndexer with
        member __.ParameterIndex(par) = dict.[par]
    }

let private toFragmentExpr (fragment : CommandFragment) =
    match fragment with
    | LocalName n -> <@@ LocalName (%%Quotations.Expr.Value(n)) @@>
    | CommandText t -> <@@ CommandText (%%Quotations.Expr.Value(t)) @@>
    | Parameter i -> <@@ Parameter (%%Quotations.Expr.Value(i)) @@>
    | Whitespace -> <@@ Whitespace @@>

let private toFragmentArrayExpr (fragments : CommandFragment IReadOnlyList) =
    Expr.NewArray(typeof<CommandFragment>, fragments |> Seq.map toFragmentExpr |> Seq.toList)

/// Lowercase initial uppercase characters.
let private toCamelCase (str : string) =
    Regex.Replace(str, @"^\p{Lu}+", fun m -> m.Value.ToLowerInvariant())

let private toRowTypeName (name : string) =
    // Must sanitize to remove things like * from the name.
    Regex.Replace(name, @"[^_a-zA-Z0-9]", fun m -> string (char (int m.Value.[0] % 26 + int 'A'))) + "Row"

type private BlueprintKeyAttributeData() =
    inherit CustomAttributeData()
    static let keyTy = typeof<BlueprintKeyAttribute>
    override __.Constructor = keyTy.GetConstructor(Type.EmptyTypes)
    override __.ConstructorArguments = [||] :> IList<_>
    override __.NamedArguments = [||] :> IList<_>

type private BlueprintColumnNameAttributeData(name : string) =
    inherit CustomAttributeData()
    static let colTy = typeof<BlueprintColumnNameAttribute>
    override __.Constructor = colTy.GetConstructor([| typeof<string> |])
    override __.ConstructorArguments =
        [|  CustomAttributeTypedArgument(typeof<string>, name)
        |] :> IList<_>
    override __.NamedArguments = [||] :> IList<_>

let rec private generateRowTypeFromColumns (model : UserModel) name (columnMap : CompileTimeColumnMap) =
    let ty =
        ProvidedTypeDefinition
            ( name
            , Some typeof<obj>
            , IsErased = false
            , HideObjectMethods = true
            )
    let fields = ResizeArray()
    let addField pk (name : string) (fieldTy : Type) =
        let fieldTy, propName =
            if name.EndsWith("*") then
                typedefof<_ IReadOnlyList>.MakeGenericType(fieldTy), name.Substring(0, name.Length - 1)
            elif name.EndsWith("?") then
                typedefof< _ option>.MakeGenericType(fieldTy), name.Substring(0, name.Length - 1)
            else fieldTy, name
        let camel = toCamelCase propName
        let field = ProvidedField("_" + camel, fieldTy)
        field.SetFieldAttributes(FieldAttributes.Private)
        let getter = ProvidedProperty(propName, fieldTy)
        if pk then
            getter.AddCustomAttribute(BlueprintKeyAttributeData())
        if name <> propName then
            getter.AddCustomAttribute(BlueprintColumnNameAttributeData(name))
        getter.GetterCode <-
            function
            | [ this ] -> Expr.FieldGet(this, field)
            | _ -> failwith "Invalid getter argument list"
        ty.AddMembers [ field :> MemberInfo; getter :> _ ]
        fields.Add(camel, field)
    for KeyValue(name, (_, column)) in columnMap.Columns do
        let info = column.Expr.Info
        addField info.PrimaryKey name info.Type.CLRType
    for KeyValue(name, subMap) in columnMap.SubMaps do
        let subTy = generateRowTypeFromColumns model (toRowTypeName name) subMap
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

let private generateRowType (model : UserModel) (name : string) (query : ColumnType QueryExprInfo) =
    CompileTimeColumnMap.Parse(query.Columns)
    |> generateRowTypeFromColumns model name

let private generateCommandMethod
    (generate : GenerateType) (command : CommandEffect) (retTy : Type) (callMeth : MethodInfo) =
    let backend = generate.UserModel.Backend
    let parameters = command.Parameters |> Seq.sortBy fst |> Seq.toList
    let indexer = parameterIndexer (parameters |> Seq.map fst)
    let fragments = backend.ToCommandFragments(indexer, command.Statements)
    let methodParameters =
        [ for NamedParameter name, ty in parameters ->
            ProvidedParameter(name.Value, ty.CLRType)
        ]
    let meth = ProvidedMethod("Command", methodParameters, retTy)
    meth.SetMethodAttrs(MethodAttributes.Static ||| MethodAttributes.Public)
    meth.InvokeCode <-
        fun args ->
            let arr =
                Expr.NewArray
                    ( typeof<obj * DbType>
                    , (args, parameters) ||> List.map2 (fun ex (_, ty) ->
                        let tx = backend.ParameterTransform(ty)
                        Expr.NewTuple([ tx.ValueTransform ex; Quotations.Expr.Value(tx.ParameterType) ]))
                    )
            let frags = toFragmentArrayExpr fragments
            Expr.CallUnchecked(callMeth, [ frags; arr ])
    meth

let generateSQLType (generate : GenerateType) (sql : string) =
    let commandEffect = CommandEffect.OfSQL(generate.UserModel.Model, generate.TypeName, sql)
    let commandCtor = typeof<CommandConstructor>
    let cmd (r : Type) = typedefof<_ Command>.MakeGenericType(r)
    let lst (r : Type) = typedefof<_ IReadOnlyList>.MakeGenericType(r)
    let rowTypes, commandCtorMethod, commandType =
        let genRowType = generateRowType generate.UserModel
        match commandEffect.ResultSets |> Seq.toList with
        | [] ->
            []
            , commandCtor.GetMethod("Command0")
            , cmd typeof<unit>
        | [ resultSet ] ->
            let rowType = genRowType "Row" resultSet
            [ rowType ]
            , commandCtor.GetMethod("Command1").MakeGenericMethod(lst rowType)
            , cmd (lst rowType)
        | [ resultSet1; resultSet2 ] ->
            let rowType1 = genRowType "Row1" resultSet1
            let rowType2 = genRowType "Row2" resultSet2
            [ rowType1; rowType2 ]
            , commandCtor.GetMethod("Command2").MakeGenericMethod(lst rowType1, lst rowType2)
            , cmd <| typedefof<ResultSets<_, _>>.MakeGenericType(lst rowType1, lst rowType2)
        | [ resultSet1; resultSet2; resultSet3 ] ->
            let rowType1 = genRowType "Row1" resultSet1
            let rowType2 = genRowType "Row2" resultSet2
            let rowType3 = genRowType "Row3" resultSet3
            [ rowType1; rowType2; rowType3 ]
            , commandCtor.GetMethod("Command3").MakeGenericMethod(lst rowType1, lst rowType2, lst rowType3)
            , cmd <| typedefof<ResultSets<_, _, _>>.MakeGenericType(lst rowType1, lst rowType2, lst rowType3)
        | sets ->
            failwithf "Too many (%d) result sets from command." (List.length sets)
    let provided =
        ProvidedTypeDefinition
            ( generate.Assembly
            , generate.Namespace
            , generate.TypeName
            , Some typeof<obj>
            , IsErased = false
            , HideObjectMethods = true
            )
    provided.AddMembers rowTypes
    provided.AddMember <| generateCommandMethod generate commandEffect commandType commandCtorMethod
    provided

let generateModelType (generate : GenerateType) =
    let provided =
        ProvidedTypeDefinition
            ( generate.Assembly
            , generate.Namespace
            , generate.TypeName
            , Some typeof<obj>
            , IsErased = false
            , HideObjectMethods = true
            )
    let migrationsField =
        ProvidedField
            ( "_migrations"
            , typeof<string Migrations.MigrationMajorVersion array>
            )
    migrationsField.SetFieldAttributes(FieldAttributes.Static ||| FieldAttributes.Private)
    provided.AddMember <| migrationsField
    let staticCtor =
        ProvidedConstructor([], IsTypeInitializer = true)
    staticCtor.InvokeCode <- fun _ ->
        Expr.FieldSet
            ( migrationsField
            , Expr.NewArray
                ( typeof<string Migrations.MigrationMajorVersion>
                , generate.UserModel.Migrations |> Seq.map Migrations.quotationize |> Seq.toList
                ))
    provided.AddMember <| staticCtor
    provided.AddMember <|
        ProvidedProperty
            ( "Migrations"
            , typeof<string Migrations.MigrationMajorVersion IReadOnlyList>
            , GetterCode = fun _ -> Expr.FieldGet(migrationsField)
            , IsStatic = true
            )
    provided

let generateType (generate : GenerateType) =
    match generate.Case with
    | GenerateSQL sql -> generateSQLType generate sql
    | GenerateModel -> generateModelType generate