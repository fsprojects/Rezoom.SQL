module private Rezoom.SQL.Provider.TypeGeneration
open System
open System.Collections.Generic
open System.Text.RegularExpressions
open System.Reflection
open FSharp.Core.CompilerServices
open FSharp.Quotations
open FSharp.Reflection
open Microsoft.Extensions.Configuration
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
        ProvidedMethod("get_ScalarValue", [], field.FieldType, invokeCode =
            function
            | [ this ] -> Expr.FieldGetUnchecked(this, field)
            | _ -> bug "Invalid getter argument list")
    let flags =
        MethodAttributes.Virtual
        ||| MethodAttributes.Private
        ||| MethodAttributes.Final
        ||| MethodAttributes.NewSlot
        ||| MethodAttributes.HasSecurity
    getterMethod.SetMethodAttrs(flags)
    // Use ProvidedTypeBuilder.MakeGenericType so that MLC-flavoured user types
    // (e.g. a UserId DU loaded via MetadataLoadContext) produce a TypeSymbol
    // instead of a TypeBuilderInstantiation — the latter throws NotSupportedException
    // from GetMethod and breaks the IL emit pass.
    let scalarInterface = ProvidedTypeBuilder.MakeGenericType(typedefof<_ IScalar>, [field.FieldType])
    let getScalarValue = scalarInterface.GetMethod("get_ScalarValue")
    ty.AddInterfaceImplementation(scalarInterface)
    ty.DefineMethodOverride(getterMethod, getScalarValue)
    ty.AddMember(getterMethod)
    
let rec private generateRowTypeFromColumns isRoot (model : UserModel) name (columnMap : CompileTimeColumnMap) =
    let ty =
        ProvidedTypeDefinition
            ( name
            , Some typeof<obj>
            , isErased = false
            , hideObjectMethods = true
            )
    ty.AddCustomAttribute(SerializableAttributeData())
    if isRoot && not columnMap.HasSubMaps then
        ty.AddCustomAttribute(BlueprintNoKeyAttributeData())
    let fields = ResizeArray()
    let addField pk (name : string) (fieldTy : Type) =
        let fieldTy, propName =
            if name.EndsWith("*") then
                ProvidedTypeBuilder.MakeGenericType(typedefof<_ IReadOnlyList>, [fieldTy]), name.Substring(0, name.Length - 1)
            elif name.EndsWith("?") then
                ProvidedTypeBuilder.MakeGenericType(typedefof<_ option>, [fieldTy]), name.Substring(0, name.Length - 1)
            else fieldTy, name
        let camel = toCamelCase propName
        let field = ProvidedField("_" + camel, fieldTy)
        field.SetFieldAttributes(FieldAttributes.Private)
        let getter = ProvidedProperty(propName, fieldTy, getterCode = function
            | [ this ] -> Expr.FieldGetUnchecked(this, field)
            | _ -> bug "Invalid getter argument list")
        if pk then
            getter.AddCustomAttribute(BlueprintKeyAttributeData())
        if name <> propName then
            getter.AddCustomAttribute(BlueprintColumnNameAttributeData(name))
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
    let ctor =
        ProvidedConstructor
            (ctorParams
            , invokeCode =
                function
                | this :: pars ->
                    Seq.zip fields pars
                    |> Seq.fold
                        (fun exp ((_, field), par) -> Expr.Sequential(exp, Expr.FieldSetUnchecked(this, field, par)))
                        (Quotations.Expr.Value(()))
                | _ -> bug "Invalid ctor argument list")
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

/// Build a quotation expression for the Backend DU case configured in rzsql.json.
/// One branch per case so F# emits the appropriate Expr.NewUnionCase under the
/// hood; that's what the ProvidedTypes IL emitter knows how to translate.
let private backendCaseExpr (config : Config.Config) =
    match config.Backend with
    | Backend.RzSQL -> <@@ Backend.RzSQL @@>
    | Backend.SQLite -> <@@ Backend.SQLite @@>
    | Backend.TSQL -> <@@ Backend.TSQL @@>
    | Backend.Postgres -> <@@ Backend.Postgres @@>

let private generateCommandMethod
    (generate : GenerateType) (command : CommandEffect) (retTy : Type) (callMeth : MethodInfo) =
    let backend = generate.UserModel.Backend
    let parameters = command.Parameters |> Seq.sortBy fst |> Seq.toList
    let indexer = parameterIndexer (parameters |> Seq.map fst)
    let commandData =
        let userTypes =
            if generate.UserModel.UserTypeLibrary.IsEmpty then <@@ None : FreezeDry.FreezeDriedUserTypeLibrary option @@>
            else
                let quoted = FreezeDry.FreezeDriedUserTypeLibrary.Of(generate.UserModel.UserTypeLibrary).Quote()
                <@@ Some %%quoted : FreezeDry.FreezeDriedUserTypeLibrary option @@>
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
                Backend = %%(backendCaseExpr generate.UserModel.Config)
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
                UserTypeLibrary = %%userTypes
            } @@>
    let useOptional = generate.UserModel.Config.Optionals = Config.FsStyle
    let methodParameters =
        [ for NamedParameter name, ty in parameters ->
            ProvidedParameter(name.Value, ty.CLRType(useOptional))
        ]
    let meth = ProvidedMethod("Command", methodParameters, retTy, isStatic = true, invokeCode = fun args ->
            let arr =
                Expr.NewArray
                    ( typeof<CommandParameter>
                    , (args, parameters) ||> List.map2 (fun ex (_, ty) ->
                        match ty.Type with
                        | ListType elemTy ->
                            let elemColType = { ty with Type = elemTy }
                            let tx = backend.ParameterTransform(elemColType)
                            let dbType = Quotations.Expr.Value(tx.ParameterType)
                            let inputArr = Expr.Coerce(ex, typeof<Array>)
                            // Coerce each element to the actual element CLR type,
                            // so backend always sees a typed Expr like it does
                            // in the scalar path.
                            let elemClrTy = elemColType.CLRType(useOptional)
                            let lambda =
                                let var = Var("x", typeof<obj>)
                                let typedElem = Expr.Coerce(Expr.Var(var), elemClrTy)
                                Expr.Lambda(var, tx.ValueTransform typedElem)
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
            Expr.CallUnchecked(callMeth, [ commandData; arr ]))
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
        | _ -> ProvidedTypeBuilder.MakeGenericType(typedefof<_ IReadOnlyList>, [rowType])
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
            , ProvidedTypeBuilder.MakeGenericMethod(commandCtor.GetMethod("Command1"), [lst resultSet rowType])
            , cmd (lst resultSet rowType)
        | [ resultSet1; resultSet2 ] ->
            let rowType1 = genRowType "Row1" resultSet1
            let rowType2 = genRowType "Row2" resultSet2
            [ rowType1; rowType2 ]
            , ProvidedTypeBuilder.MakeGenericMethod(commandCtor.GetMethod("Command2"), [lst resultSet1 rowType1; lst resultSet2 rowType2])
            , cmd <| ProvidedTypeBuilder.MakeGenericType(typedefof<ResultSets<_, _>>, [lst resultSet1 rowType1; lst resultSet2 rowType2])
        | [ resultSet1; resultSet2; resultSet3 ] ->
            let rowType1 = genRowType "Row1" resultSet1
            let rowType2 = genRowType "Row2" resultSet2
            let rowType3 = genRowType "Row3" resultSet3
            [ rowType1; rowType2; rowType3 ]
            , ProvidedTypeBuilder.MakeGenericMethod(commandCtor.GetMethod("Command3"), [lst resultSet1 rowType1; lst resultSet2 rowType2; lst resultSet3 rowType3])
            , cmd <| ProvidedTypeBuilder.MakeGenericType(typedefof<ResultSets<_, _, _>>, [lst resultSet1 rowType1; lst resultSet2 rowType2; lst resultSet3 rowType3])
        | [ resultSet1; resultSet2; resultSet3; resultSet4 ] ->
            let rowType1 = genRowType "Row1" resultSet1
            let rowType2 = genRowType "Row2" resultSet2
            let rowType3 = genRowType "Row3" resultSet3
            let rowType4 = genRowType "Row4" resultSet4
            [ rowType1; rowType2; rowType3; rowType4 ]
            , ProvidedTypeBuilder.MakeGenericMethod(commandCtor.GetMethod("Command4"), [lst resultSet1 rowType1; lst resultSet2 rowType2; lst resultSet3 rowType3; lst resultSet4 rowType4])
            , cmd <|
                ProvidedTypeBuilder.MakeGenericType(typedefof<ResultSets<_, _, _, _>>, [lst resultSet1 rowType1; lst resultSet2 rowType2; lst resultSet3 rowType3; lst resultSet4 rowType4])
        | sets ->
            fail <| Error.commandContainsTooManyResultSets (List.length sets)
    let provided =
        ProvidedTypeDefinition
            ( generate.Assembly
            , generate.Namespace
            , generate.TypeName
            , Some typeof<obj>
            , isErased = false
            , hideObjectMethods = true
            )
    provided.AddXmlDocDelayed (fun () -> DocStrings.commandEffectDocString commandEffect)
    provided.AddMembers rowTypes
    provided.AddMember <| generateCommandMethod generate commandEffect commandType commandCtorMethod
    provided

let generateMigrationMembers
    (config : Config.Config) (backend : IBackend) (provided : ProvidedTypeDefinition) migrationProperty =
    // Two Migrate overloads:
    //   Migrate(MigrationConfig, IServiceProvider) reads the connection string
    //     from a registered IConfiguration. The backend's canonical ADO.NET
    //     driver invariant is used (Microsoft.Data.Sqlite, Npgsql, etc.).
    //   Migrate(MigrationConfig, string) takes the connection string directly,
    //     intended for standalone migrator tools that don't want IConfiguration.
    // Migration is its own concern: it doesn't go through ConnectionProvider
    // even when one is registered. Users with custom ConnectionProviders who
    // need migration to honor their routing should use the connection-string
    // overload and supply the string themselves.
    // Note: NOT `ignore <| try ... finally ...` and NOT a unit-typed try block;
    // ProvidedTypes' IL translator generates invalid IL for those shapes. The
    // `let _ = try ... 0 finally ...; ()` form works around it.
    let connectionName = Quotations.Expr.Value(config.ConnectionName)
    let providerInvariant =
        // Resolve the canonical driver string at codegen time from the configured
        // Backend DU case, then embed the resulting string as a quotation constant.
        Quotations.Expr.Value(Backend.canonicalDriver config.Backend)
    let runMigration backendInstanceExpr configExpr =
        <@@ let migrations : string MigrationTree array = %%Expr.PropertyGet(migrationProperty)
            let backendInstance : IMigrationBackend = %%backendInstanceExpr
            try
                MigrationUtilities.runMigrations (%%configExpr) backendInstance migrations
            finally
                backendInstance.Dispose()
        @@>
    let buildInfoExpr connectionStringExpr =
        <@@ {   Name = (%%connectionName : string)
                ConnectionString = (%%connectionStringExpr : string)
                ProviderName = (%%providerInvariant : string)
            } @@>
    do
        let pars =
            [   ProvidedParameter("config", typeof<MigrationConfig>)
                ProvidedParameter("services", typeof<IServiceProvider>)
            ]
        let meth = ProvidedMethod("Migrate", pars, typeof<unit>, isStatic = true, invokeCode = function
            | [ configArg; services ] ->
                let backendInstance =
                    <@@ let svc = (%%services : IServiceProvider)
                        let cfg = svc.GetService(typeof<IConfiguration>) :?> IConfiguration
                        if isNull cfg then
                            failwith
                                "Migrate(MigrationConfig, IServiceProvider) needs an IConfiguration \
                                 registered in the service provider. Either register one (standard \
                                 for ASP.NET Core / generic host) or use the \
                                 Migrate(MigrationConfig, connectionString) overload."
                        let connStr = cfg.[sprintf "ConnectionStrings:%s" (%%connectionName : string)]
                        if System.String.IsNullOrEmpty(connStr) then
                            failwithf
                                "No connection string named '%s' in configuration (looked for ConnectionStrings:%s)"
                                (%%connectionName : string) (%%connectionName : string)
                        let info : ConnectionInfo =
                            {   Name = (%%connectionName : string)
                                ConnectionString = connStr
                                ProviderName = (%%providerInvariant : string)
                            }
                        (%backend.MigrationBackend) info
                    @@>
                runMigration backendInstance configArg
            | _ -> bug "Invalid migrate argument list")
        provided.AddMember meth
    do
        let pars =
            [   ProvidedParameter("config", typeof<MigrationConfig>)
                ProvidedParameter("connectionString", typeof<string>)
            ]
        let meth = ProvidedMethod("Migrate", pars, typeof<unit>, isStatic = true, invokeCode = function
            | [ configArg; connectionString ] ->
                let backendInstance =
                    <@@ let info : ConnectionInfo = %%buildInfoExpr connectionString
                        (%backend.MigrationBackend) info
                    @@>
                runMigration backendInstance configArg
            | _ -> bug "Invalid migrate argument list")
        provided.AddMember meth

let generateModelType (generate : GenerateType) =
    let backend = generate.UserModel.Backend
    let provided =
        ProvidedTypeDefinition
            ( generate.Assembly
            , generate.Namespace
            , generate.TypeName
            , Some typeof<obj>
            , isErased = false
            , hideObjectMethods = true
            )
    let migrationsProperty = ProvidedProperty("migrations", typeof<string MigrationTree array>, (fun _ ->
      Expr.NewArray
        ( typeof<string MigrationTree>
        , generate.UserModel.Migrations
            |> Seq.map MigrationUtilities.quotationizeMigrationTree
            |> Seq.toList
        )), isStatic = true)
    provided.AddMember <| migrationsProperty
    generateMigrationMembers generate.UserModel.Config backend provided migrationsProperty
    provided

let generateType (generate : GenerateType) =
    match generate.Case with
    | GenerateSQL sql -> generateSQLType generate sql
    | GenerateModel -> generateModelType generate