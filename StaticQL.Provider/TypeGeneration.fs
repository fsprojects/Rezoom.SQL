module StaticQL.Provider.TypeGeneration
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
open StaticQL.Mapping
open StaticQL

type GenerateType =
    {
        UserModel : UserModel
        Assembly : Assembly
        Namespace : string
        TypeName : string
        Command : CommandEffect
    }

let private parameterIndexer (pars : BindParameter seq) =
    let dict =
        pars |> Seq.indexed |> Seq.map (fun (a, b) -> (b, a)) |> dict
    { new IParameterIndexer with
        member __.ParameterIndex(par) = dict.[par]
    }

let private localNameCase, private commandTextCase, private parameterCase, private whiteCase =
    let cases = FSharpType.GetUnionCases(typeof<CommandFragment>)
    ( cases |> Array.find (fun c -> c.Name = "LocalName")
    , cases |> Array.find (fun c -> c.Name = "CommandText")
    , cases |> Array.find (fun c -> c.Name = "Parameter")
    , cases |> Array.find (fun c -> c.Name = "Whitespace")
    )

let private toFragmentExpr (fragment : CommandFragment) =
    match fragment with
    | LocalName n -> Expr.NewUnionCase(localNameCase, [ Quotations.Expr.Value(n) ])
    | CommandText t -> Expr.NewUnionCase(commandTextCase, [ Quotations.Expr.Value(t) ])
    | Parameter i -> Expr.NewUnionCase(parameterCase, [ Quotations.Expr.Value(i) ])
    | Whitespace -> Expr.NewUnionCase(whiteCase, [])

let private toFragmentArrayExpr (fragments : CommandFragment IReadOnlyList) =
    Expr.NewArray(typeof<CommandFragment>, fragments |> Seq.map toFragmentExpr |> Seq.toList)

/// Lowercase initial uppercase characters.
let private toCamelCase (str : string) =
    Regex.Replace(str, @"^\p{Lu}+", fun m -> m.Value.ToLowerInvariant())

type KeyType() =
    [<ComponentModel.DataAnnotations.Key>]
    member __.DummyKeyProperty = 0
    static member AttributeData =
        CustomAttributeData.GetCustomAttributes(typeof<KeyType>.GetProperty("DummyKeyProperty"))
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
        addField column.Expr.Info.PrimaryKey name <| column.Expr.Info.Type.CLRType
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

let private generateRowType (model : UserModel) (name : string) (query : ColumnType QueryExprInfo) =
    CompileTimeColumnMap.Parse(query.Columns)
    |> generateRowTypeFromColumns model name

let private generateCommandMethod (generate : GenerateType) (retTy : Type) (callMeth : MethodInfo) =
    let backend = generate.UserModel.Backend
    let parameters = generate.Command.Parameters |> Seq.sortBy fst |> Seq.toList
    let indexer = parameterIndexer (parameters |> Seq.map fst)
    let fragments = backend.ToCommandFragments(indexer, generate.Command.Statements)
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

let generateType (generate : GenerateType) =
    let commandCtor = typeof<CommandConstructor>
    let cmd (r : Type) = typedefof<_ Command>.MakeGenericType(r)
    let rowTypes, commandCtorMethod, commandType =
        let genRowType = generateRowType generate.UserModel
        match generate.Command.ResultSets |> Seq.toList with
        | [] ->
            []
            , commandCtor.GetMethod("Command0")
            , cmd typeof<unit>
        | [ resultSet ] ->
            let rowType = genRowType "Row" resultSet
            [ rowType ]
            , commandCtor.GetMethod("Command1").MakeGenericMethod(rowType)
            , cmd rowType
        | [ resultSet1; resultSet2 ] ->
            let rowType1 = genRowType "Row1" resultSet1
            let rowType2 = genRowType "Row2" resultSet2
            [ rowType1; rowType2 ]
            , commandCtor.GetMethod("Command2").MakeGenericMethod(rowType1, rowType2)
            , cmd <| typedefof<ResultSets<_, _>>.MakeGenericType(rowType1, rowType2)
        | [ resultSet1; resultSet2; resultSet3 ] ->
            let rowType1 = genRowType "Row1" resultSet1
            let rowType2 = genRowType "Row2" resultSet2
            let rowType3 = genRowType "Row3" resultSet3
            [ rowType1; rowType2; rowType3 ]
            , commandCtor.GetMethod("Command3").MakeGenericMethod(rowType1, rowType2, rowType3)
            , cmd <| typedefof<ResultSets<_, _, _>>.MakeGenericType(rowType1, rowType2, rowType3)
        | sets ->
            failwithf "Too many (%d) result sets from command." (List.length sets)
    let provided =
        ProvidedTypeDefinition
            (generate.Assembly, generate.Namespace, generate.TypeName, Some typeof<obj>, IsErased = false)
    provided.AddMembers rowTypes
    provided.AddMember <| generateCommandMethod generate commandType commandCtorMethod
    provided