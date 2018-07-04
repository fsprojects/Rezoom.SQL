module private Rezoom.SQL.Provider.TypeGeneration
open System
open System.Collections.Generic
open System.Text.RegularExpressions
open System.Reflection
open FSharp.Core.CompilerServices
open FSharp.Quotations
open FSharp.Reflection
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypes.UncheckedQuotations
open Rezoom
open Rezoom.SQL
open Rezoom.SQL.Mapping
open Rezoom.SQL.Migrations
open Rezoom.SQL.Compiler

type GenerateTypeCase =
    | GenerateSQL of string
    | GenerateModel

type GenerateType =
    {   UserModel : UserModel
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
    | InlineParameter (dbType, o) ->
        // won't work with complex types so watch out
        <@@ InlineParameter (%%Quotations.Expr.Value(dbType), %%Quotations.Expr.Value(o)) @@>
    | Whitespace -> <@@ Whitespace @@>
    | LineBreak -> <@@ LineBreak @@>
    | Indent -> <@@ Indent @@>
    | Outdent -> <@@ Outdent @@>

let private toFragmentArrayExpr (fragments : CommandFragment IReadOnlyList) =
    Expr.NewArray(typeof<CommandFragment>, fragments |> Seq.map toFragmentExpr |> Seq.toList)

/// Lowercase initial uppercase characters.
let private toCamelCase (str : string) =
    Regex.Replace(str, @"^\p{Lu}+", fun m -> m.Value.ToLowerInvariant())

let private toRowTypeName (name : string) =
    // Must sanitize to remove things like * from the name.
    Regex.Replace(name, @"[^_a-zA-Z0-9]", fun m -> string (char (int m.Value.[0] % 26 + int 'A'))) + "Row"

type private SerializableAttributeData() =
    inherit CustomAttributeData()
    override __.Constructor = typeof<SerializableAttribute>.GetConstructor(Type.EmptyTypes)
    override __.ConstructorArguments = [||] :> IList<_>
    override __.NamedArguments = [||] :> IList<_>

type private BlueprintNoKeyAttributeData() =
    inherit CustomAttributeData()
    override __.Constructor = typeof<BlueprintNoKeyAttribute>.GetConstructor(Type.EmptyTypes)
    override __.ConstructorArguments = [||] :> IList<_>
    override __.NamedArguments = [||] :> IList<_>

type private BlueprintKeyAttributeData() =
    inherit CustomAttributeData()
    override __.Constructor = typeof<BlueprintKeyAttribute>.GetConstructor(Type.EmptyTypes)
    override __.ConstructorArguments = [||] :> IList<_>
    override __.NamedArguments = [||] :> IList<_>

type private BlueprintColumnNameAttributeData(name : string) =
    inherit CustomAttributeData()
    override __.Constructor = typeof<BlueprintColumnNameAttribute>.GetConstructor([| typeof<string> |])
    override __.ConstructorArguments =
        [|  CustomAttributeTypedArgument(typeof<string>, name)
        |] :> IList<_>
    override __.NamedArguments = [||] :> IList<_>

let private addScalarInterface (ty : ProvidedTypeDefinition) (field : ProvidedField) =
    let getterMethod =
        ProvidedMethod("get_ScalarValue", [], field.FieldType, InvokeCode =
            function
            | [ this ] -> Expr.FieldGet(this, field)
            | _ -> bug "Invalid getter argument list")
    let flags =
        MethodAttributes.Virtual
        ||| MethodAttributes.Private
        ||| MethodAttributes.Final
        ||| MethodAttributes.NewSlot
        ||| MethodAttributes.HasSecurity
    getterMethod.SetMethodAttrs(flags)
    let scalarInterface = typedefof<_ IScalar>.MakeGenericType(field.FieldType)
    let getScalarValue = scalarInterface.GetMethod("get_ScalarValue")
    ty.AddInterfaceImplementation(scalarInterface)
    ty.DefineMethodOverride(getterMethod, getScalarValue)
    ty.AddMember(getterMethod)

let rec private generateRowTypeFromColumns isRoot (model : UserModel) name (columnMap : CompileTimeColumnMap) =
    let ty =
        ProvidedTypeDefinition
            ( name
            , Some typeof<obj>
            , IsErased = false
            , HideObjectMethods = true
            )
    ty.AddCustomAttribute(SerializableAttributeData())
    if isRoot && not columnMap.HasSubMaps then
        ty.AddCustomAttribute(BlueprintNoKeyAttributeData())
    let fields = ResizeArray()
    let addField pk (name : string) (fieldTy : Type) =
        let fieldTy, propName =
            if name.EndsWith("*") then
                typedefof<_ IReadOnlyList>.MakeGenericType(fieldTy), name.Substring(0, name.Length - 1)
            elif name.EndsWith("?") then
                typedefof<_ option>.MakeGenericType(fieldTy), name.Substring(0, name.Length - 1)
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
            | _ -> bug "Invalid getter argument list"
        ty.AddMembers [ field :> MemberInfo; getter :> _ ]
        fields.Add(camel, field)
    for KeyValue(name, (_, column)) in columnMap.Columns do
        let info = column.Expr.Info
        addField info.PrimaryKey name <| info.Type.CLRType(useOptional = (model.Config.Optionals = Config.FsStyle))
    for KeyValue(name, subMap) in columnMap.SubMaps do
        let subTy = generateRowTypeFromColumns false model (toRowTypeName name) subMap
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
        | _ -> bug "Invalid ctor argument list"
    ty.AddMember(ctor)
    if fields.Count = 1 then
        addScalarInterface ty (snd fields.[0])
    ty

let private generateRowType (model : UserModel) (name : string) (query : ColumnType QueryExprInfo) =
    CompileTimeColumnMap.Parse(query.Columns)
    |> generateRowTypeFromColumns true model name

let private maskOfTables (model : UserModel) (tables : QualifiedObjectName seq) =
    let mutable mask = BitMask.Zero
    for table in tables do
        match model.TableIds.Value |> Map.tryFind table with
        | None -> ()
        | Some id ->
            mask <- mask.WithBit(id % 128, true)
    mask

let private generateCommandMethod
    (generate : GenerateType) (command : CommandEffect) (retTy : Type) (callMeth : MethodInfo) =
    let backend = generate.UserModel.Backend
    let parameters = command.Parameters |> Seq.sortBy fst |> Seq.toList
    let indexer = parameterIndexer (parameters |> Seq.map fst)
    let commandData =
        let fragments = backend.ToCommandFragments(indexer, command.Statements) |> toFragmentArrayExpr
        let identity = generate.Namespace + generate.TypeName
        let resultSetCount = command.ResultSets() |> Seq.length
        let cacheable, dependencies, invalidations =
            match command.CacheInfo.Value with
            | Some info ->
                ( info.Idempotent
                , maskOfTables generate.UserModel info.ReadTables
                , maskOfTables generate.UserModel info.WriteTables
                )
            | None -> false, BitMask.Full, BitMask.Full // assume the worst
        <@@ {   ConnectionName = %%Quotations.Expr.Value(generate.UserModel.ConnectionName)
                Identity = %%Quotations.Expr.Value(identity)
                Fragments = (%%fragments : _ array) :> _ IReadOnlyList
                Cacheable = %%Quotations.Expr.Value(cacheable)
                DependencyMask =
                    BitMask
                        ( %%Quotations.Expr.Value(dependencies.HighBits)
                        , %%Quotations.Expr.Value(dependencies.LowBits))
                InvalidationMask =
                    BitMask
                        ( %%Quotations.Expr.Value(invalidations.HighBits)
                        , %%Quotations.Expr.Value(invalidations.LowBits))
                ResultSetCount = Some (%%Quotations.Expr.Value(resultSetCount))
            } @@>
    let useOptional = generate.UserModel.Config.Optionals = Config.FsStyle
    let methodParameters =
        [ for NamedParameter name, ty in parameters ->
            ProvidedParameter(name.Value, ty.CLRType(useOptional))
        ]
    let meth = ProvidedMethod("Command", methodParameters, retTy)
    meth.SetMethodAttrs(MethodAttributes.Static ||| MethodAttributes.Public)
    meth.InvokeCode <-
        fun args ->
            let arr =
                Expr.NewArray
                    ( typeof<CommandParameter>
                    , (args, parameters) ||> List.map2 (fun ex (_, ty) ->
                        match ty.Type with
                        | ListType elemTy ->
                            let tx = backend.ParameterTransform({ ty with Type = elemTy })
                            let dbType = Quotations.Expr.Value(tx.ParameterType)
                            let inputArr = Expr.Coerce(ex, typeof<Array>)
                            let lambda =
                                let var = Var("x", typeof<obj>)
                                Expr.Lambda(var, tx.ValueTransform (Expr.Var(var)))
                            let arr =
                                <@@ [| for ex in ((%%inputArr) : Array) -> ((%%lambda) : obj -> obj) ex |] @@>
                            <@@ ListParameter(%%dbType, %%Expr.Coerce(arr, typeof<Array>)) @@>
                        | RawSQLType ->
                            <@@ RawSQLParameter %%ex @@>
                        | _ ->
                            let tx = backend.ParameterTransform(ty)
                            let dbType = Quotations.Expr.Value(tx.ParameterType)
                            <@@ ScalarParameter(%%dbType, %%tx.ValueTransform ex) @@>)
                    )
            Expr.CallUnchecked(callMeth, [ commandData; arr ])
    meth

let validateSQLCommand (generate : GenerateType) (effect : CommandEffect) =
    match effect.ModelChange with
    | None -> ()
    | Some change ->
        let previousModel = generate.UserModel.Model
        if change = previousModel then () else
        match change.Schemas.TryFind(change.TemporarySchema)
            , previousModel.Schemas.TryFind(previousModel.TemporarySchema) with
        | Some newTemp, Some oldTemp when newTemp.Objects.Count > oldTemp.Objects.Count ->
            fail <| Error.commandLeavesTempTable
        | _ ->
            fail <| Error.commandChangesSchema

let generateSQLType (generate : GenerateType) (sql : string) =
    let commandEffect = CommandEffect.OfSQL(generate.UserModel.Model, generate.TypeName, sql)
    validateSQLCommand generate commandEffect
    let commandCtor = typeof<CommandConstructor>
    let cmd (r : Type) = typedefof<_ Command>.MakeGenericType(r)
    let lst (query : _ QueryExprInfo) (rowType : Type) =
        match query.StaticRowCount with
        | Some 1 -> rowType
        | _ -> typedefof<_ IReadOnlyList>.MakeGenericType(rowType)
    let rowTypes, commandCtorMethod, commandType =
        let genRowType = generateRowType generate.UserModel
        match commandEffect.ResultSets() |> Seq.toList with
        | [] ->
            []
            , commandCtor.GetMethod("Command0")
            , cmd typeof<unit>
        | [ resultSet ] ->
            let rowType = genRowType "Row" resultSet
            [ rowType ]
            , commandCtor.GetMethod("Command1").MakeGenericMethod(lst resultSet rowType)
            , cmd (lst resultSet rowType)
        | [ resultSet1; resultSet2 ] ->
            let rowType1 = genRowType "Row1" resultSet1
            let rowType2 = genRowType "Row2" resultSet2
            [ rowType1; rowType2 ]
            , commandCtor.GetMethod("Command2").MakeGenericMethod(lst resultSet1 rowType1, lst resultSet2 rowType2)
            , cmd <| typedefof<ResultSets<_, _>>.MakeGenericType(lst resultSet1 rowType1, lst resultSet2 rowType2)
        | [ resultSet1; resultSet2; resultSet3 ] ->
            let rowType1 = genRowType "Row1" resultSet1
            let rowType2 = genRowType "Row2" resultSet2
            let rowType3 = genRowType "Row3" resultSet3
            [ rowType1; rowType2; rowType3 ]
            , commandCtor.GetMethod("Command3").MakeGenericMethod
                (lst resultSet1 rowType1, lst resultSet2 rowType2, lst resultSet3 rowType3)
            , cmd <| typedefof<ResultSets<_, _, _>>.MakeGenericType
                (lst resultSet1 rowType1, lst resultSet2 rowType2, lst resultSet3 rowType3)
        | [ resultSet1; resultSet2; resultSet3; resultSet4 ] ->
            let rowType1 = genRowType "Row1" resultSet1
            let rowType2 = genRowType "Row2" resultSet2
            let rowType3 = genRowType "Row3" resultSet3
            let rowType4 = genRowType "Row4" resultSet4
            [ rowType1; rowType2; rowType3; rowType4 ]
            , commandCtor.GetMethod("Command4").MakeGenericMethod
                (lst resultSet1 rowType1, lst resultSet2 rowType2, lst resultSet3 rowType3, lst resultSet4 rowType4)
            , cmd <|
                typedefof<ResultSets<_, _, _, _>>.MakeGenericType
                    (lst resultSet1 rowType1, lst resultSet2 rowType2, lst resultSet3 rowType3, lst resultSet4 rowType4)
        | sets ->
            fail <| Error.commandContainsTooManyResultSets (List.length sets)
    let provided =
        ProvidedTypeDefinition
            ( generate.Assembly
            , generate.Namespace
            , generate.TypeName
            , Some typeof<obj>
            , IsErased = false
            , HideObjectMethods = true
            )
    provided.AddXmlDocDelayed (fun () -> DocStrings.commandEffectDocString commandEffect)
    provided.AddMembers rowTypes
    provided.AddMember <| generateCommandMethod generate commandEffect commandType commandCtorMethod
    provided

let generateMigrationMembers
    (config : Config.Config) (backend : IBackend) (provided : ProvidedTypeDefinition) migrationsField =
    do
        let pars =
            [   ProvidedParameter("config", typeof<MigrationConfig>)
                ProvidedParameter("connectionName", typeof<string>)
            ]
        let meth = ProvidedMethod("Migrate", pars, typeof<unit>)
        meth.IsStaticMethod <- true
        meth.InvokeCode <- function
            | [ config; connectionName ] -> 
                let backend =
                    <@ fun () ->
                        (%backend.MigrationBackend)
                            (DefaultConnectionProvider.ResolveConnectionString(%%connectionName))
                    @>
                <@@ let migrations : string MigrationTree array = %%Expr.FieldGet(migrationsField)
                    migrations.Run(%%config, %%(upcast backend))
                @@>
            | _ -> bug "Invalid migrate argument list"
        provided.AddMember meth
    do
        let connectionName = Quotations.Expr.Value(config.ConnectionName)
        let pars =
            [   ProvidedParameter("config", typeof<MigrationConfig>)
            ]
        let meth = ProvidedMethod("Migrate", pars, typeof<unit>)
        meth.IsStaticMethod <- true
        meth.InvokeCode <- function
            | [ config ] ->
                let backend =
                    <@ fun () ->
                        (%backend.MigrationBackend)
                            (DefaultConnectionProvider.ResolveConnectionString(%%connectionName))
                    @>
                <@@ let migrations : string MigrationTree array = %%Expr.FieldGet(migrationsField)
                    migrations.Run(%%config, %%(upcast backend))
                @@>
            | _ -> bug "Invalid migrate argument list"
        provided.AddMember meth

let generateModelType (generate : GenerateType) =
    let backend = generate.UserModel.Backend
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
            , typeof<string MigrationTree array>
            )
    migrationsField.SetFieldAttributes(FieldAttributes.Static ||| FieldAttributes.Private)
    provided.AddMember <| migrationsField
    let staticCtor =
        ProvidedConstructor([], IsTypeInitializer = true)
    staticCtor.InvokeCode <- fun _ ->
        Expr.FieldSet
            ( migrationsField
            , Expr.NewArray
                ( typeof<string MigrationTree>
                , generate.UserModel.Migrations
                    |> Seq.map MigrationUtilities.quotationizeMigrationTree
                    |> Seq.toList
                ))
    provided.AddMember <| staticCtor
    generateMigrationMembers generate.UserModel.Config backend provided migrationsField
    provided

let generateType (generate : GenerateType) =
    match generate.Case with
    | GenerateSQL sql -> generateSQLType generate sql
    | GenerateModel -> generateModelType generate