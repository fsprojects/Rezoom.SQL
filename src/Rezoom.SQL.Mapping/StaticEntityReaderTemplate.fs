namespace Rezoom.SQL.Mapping.CodeGeneration
open Rezoom.SQL.Mapping
open LicenseToCIL
open LicenseToCIL.Stack
open LicenseToCIL.Ops
open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit

[<NoComparison>]
[<NoEquality>] 
type private EntityReaderBuilder =
    {   TemplateCacheField : TemplateCacheStaticField
        Ctor : E S * IL
        ProcessColumns : E S * IL
        ImpartKnowledge : E S * IL
        Read : E S * IL
        SetReverse : E S * IL
        ToEntity : E S * IL
    }

module private UtilityInstr =
    let (|-->) (emptyStack, il) (op : Op<_, _>) =
        op emptyStack null il
open UtilityInstr

type private StaticEntityReaderTemplate =
    static member ColumnGenerator(builder, column, templateCacheField) =
        match column.Blueprint.Value.Cardinality with
        | One { Shape = Primitive p } ->
            PrimitiveColumnGenerator(builder, column, p) :> EntityReaderColumnGenerator
        | One { Shape = Composite c } ->
            CompositeColumnGenerator(builder, column, c, templateCacheField) :> EntityReaderColumnGenerator
        | Many (element, conversion) ->
            match element.Shape with
            | Composite c when c.Identity.Count > 0 ->
                ManyEntityColumnGenerator(builder, Some column, element, conversion, templateCacheField) :> EntityReaderColumnGenerator
            | _ ->
                ManyColumnGenerator(builder, Some column, element, conversion, templateCacheField) :> EntityReaderColumnGenerator

    static member ImplementPrimitive(builder : TypeBuilder, ty : Type, primitive : Primitive, readerBuilder) =
        let info = builder.DefineField("_i", typeof<ColumnInfo>, FieldAttributes.Private)
        let value = builder.DefineField("_v", ty, FieldAttributes.Private)
        readerBuilder.Ctor |--> ret'void
        readerBuilder.ProcessColumns |-->
            cil {
                yield ldarg 0
                yield ldarg 1
                yield call1 ColumnMap.PrimaryColumnMethod
                yield stfld info
                yield ret'void
            }
        readerBuilder.ImpartKnowledge |-->
            cil {
                yield ldarg 1
                yield castclass builder
                yield ldarg 0
                yield ldfld info
                yield stfld info
                yield ret'void
            }
        readerBuilder.Read |-->
            cil {
                yield ldarg 0
                yield ldarg 1
                yield ldarg 0
                yield ldfld info
                yield generalize2 primitive.Converter
                yield stfld value
                yield ret'void
            }
        readerBuilder.SetReverse |--> ret'void
        readerBuilder.ToEntity |-->
            cil {
                yield ldarg 0
                yield ldfld value
                yield ret
            }

    static member ImplementMany(builder : TypeBuilder, element : ElementBlueprint, conversion, readerBuilder, templateCacheField) =
        let generator =
            match element.Shape with
            | Composite c when c.Identity.Count > 0 ->
                ManyEntityColumnGenerator(builder, None, element, conversion, templateCacheField) :> EntityReaderColumnGenerator
            | _ ->
                ManyColumnGenerator(builder, None, element, conversion, templateCacheField) :> EntityReaderColumnGenerator
        readerBuilder.Ctor |--> 
            cil {
                yield ldarg 0
                yield generator.DefineConstructor()
                yield pop
                yield ret'void
            }
        readerBuilder.ProcessColumns |-->
            cil {
                yield ldarg 0
                yield generator.DefineProcessColumns()
                yield pop
                yield ret'void
            }
        readerBuilder.ImpartKnowledge |-->
            cil {
                yield ldarg 0
                yield generator.DefineImpartKnowledgeToNext()
                yield pop
                yield ret'void
            }
        readerBuilder.Read |-->
            cil {
                let! lbl = deflabel
                yield ldarg 0
                yield generator.DefineRead(lbl)
                yield mark lbl
                yield pop
                yield ret'void
            }
        readerBuilder.SetReverse |--> 
            cil {
                yield ldarg 0
                yield generator.DefineSetReverse()
                yield pop
                yield ret'void
            }
        readerBuilder.ToEntity |-->
            cil {
                let! self = deflocal builder
                yield generator.DefinePush(self)
                yield ret
            }
            
    static member ImplementComposite(builder, composite : Composite, readerBuilder, templateCacheField) =
        let columns =
                [| for column in composite.Columns.Values ->
                    column, StaticEntityReaderTemplate.ColumnGenerator(builder, column, templateCacheField)
                |]
        readerBuilder.Ctor |-->
            cil {
                yield ldarg 0
                for _, column in columns do
                    yield column.DefineConstructor()
                yield pop
                yield ret'void
            }
        readerBuilder.ProcessColumns |-->
            cil {
                yield ldarg 0
                for _, column in columns do
                    yield column.DefineProcessColumns()
                yield pop
                yield ret'void
            }
        readerBuilder.ImpartKnowledge |-->
            cil {
                yield ldarg 0
                for _, column in columns do
                    yield column.DefineImpartKnowledgeToNext()
                yield pop
                yield ret'void
            }
        readerBuilder.Read |-->
            cil {
                let! skipOnes = deflabel
                let! skipAll = deflabel
                yield ldarg 0
                let ones, others = columns |> Array.partition (fun (b, _) -> b.Blueprint.Value.IsOne())
                for _, column in ones do
                    yield column.DefineRead(skipOnes)
                yield mark skipOnes
                for _, column in others do
                    yield column.DefineRead(skipAll)
                yield mark skipAll
                yield pop
                yield ret'void
            }
        readerBuilder.SetReverse |-->
            cil {
                yield ldarg 0
                for _, column in columns do
                    yield column.DefineSetReverse()
                yield pop
                yield ret'void
            }
        let constructorColumns =
            seq {
                for blue, column in columns do
                    match blue.Setter with
                    | SetConstructorParameter paramInfo ->
                        yield paramInfo.Position, column
                    | _ -> ()
            } |> Seq.sortBy fst |> Seq.map snd |> Seq.toArray
        readerBuilder.ToEntity |-->
            cil {
                let! self = deflocal builder
                if constructorColumns |> Array.exists (fun c -> c.RequiresSelfReferenceToPush) then
                    // todo call a wrapper method with framework check to invoke the right underlying
                    // https://learn.microsoft.com/en-us/dotnet/fundamentals/syslib-diagnostics/syslib0050
                    let uninit =
                        typeof<System.Runtime.Serialization.FormatterServices>.GetMethod("GetUninitializedObject")
                    yield ldtoken composite.Output
                    yield call1 (typeof<Type>.GetMethod("GetTypeFromHandle"))
                    yield call1 uninit
                    yield castclass composite.Output
                    yield dup
                    yield stloc self
                    yield dup
                    for column in constructorColumns do
                        yield column.DefinePush(self)
                        yield pretend
                    yield (fun _ _ il ->
                        il.EmitCtor(OpCodes.Call, composite.Constructor)
                        ())
                else
                    for column in constructorColumns do
                        yield column.DefinePush(self)
                        yield pretend
                    yield newobj'x composite.Constructor
                    if composite.ReferencesQueryParent then
                        yield dup
                        yield stloc self
                for blue, column in columns do
                    match blue.Setter with
                    | SetField field ->
                        yield dup
                        yield column.DefinePush(self)
                        yield stfld field
                    | SetProperty prop ->
                        yield dup
                        yield column.DefinePush(self)
                        let meth = prop.GetSetMethod()
                        yield (if meth.IsVirtual then callvirt2'void else call2'void) meth
                    | _ -> ()
                yield ret
            } |> ignore
    static member ImplementReader(blueprint : Blueprint, builder : TypeBuilder, mappings : UserTypeLibrary) =
        let readerTy = typedefof<_ EntityReader>.MakeGenericType(blueprint.Output)
        let methodAttrs = MethodAttributes.Public ||| MethodAttributes.Virtual
        let genericType = typedefof<InternalReaderTemplateCache<_>>
        let templateCacheField = Generation.makeTemplateStaticCacheField builder genericType
        let readerBuilder =
            {   TemplateCacheField = templateCacheField
                Ctor =
                    Stack.empty, IL(builder
                        .DefineConstructor(MethodAttributes.Public, CallingConventions.HasThis, Type.EmptyTypes)
                        .GetILGenerator())
                ImpartKnowledge =
                    Stack.empty, IL(builder
                        .DefineMethod("ImpartKnowledgeToNext", methodAttrs, typeof<Void>, [| readerTy |])
                        .GetILGenerator())
                ProcessColumns =
                    Stack.empty, IL(builder
                        .DefineMethod("ProcessColumns", methodAttrs, typeof<Void>, [| typeof<ColumnMap> |])
                        .GetILGenerator())
                Read = Stack.empty, IL(builder
                    .DefineMethod("Read", methodAttrs, typeof<Void>, [| typeof<Row> |]).GetILGenerator())
                SetReverse =
                    Stack.empty, IL(builder
                        .DefineMethod("SetReverse", methodAttrs, typeof<Void>, [| typeof<ColumnId>; typeof<obj> |])
                        .GetILGenerator())
                ToEntity = Stack.empty, IL(builder
                    .DefineMethod("ToEntity", methodAttrs, blueprint.Output, Type.EmptyTypes).GetILGenerator())
            }
        match blueprint.Cardinality with
        | One { Shape = Primitive primitive } ->
            StaticEntityReaderTemplate.ImplementPrimitive(builder, blueprint.Output, primitive, readerBuilder)
        | One { Shape = Composite composite } ->
            StaticEntityReaderTemplate.ImplementComposite(builder, composite, readerBuilder, templateCacheField)
        | Many (element, conversion) ->
            StaticEntityReaderTemplate.ImplementMany(builder, element, conversion, readerBuilder, templateCacheField)
        let finalTy = builder.CreateTypeInfo().AsType()
        do
            // Re-derive the closed generic type from the FINALIZED type, not the TypeBuilder.
            let closedCacheTy = templateCacheField.GenericType.MakeGenericType(finalTy)
            let instance = Activator.CreateInstance(closedCacheTy, [| box mappings |])
            let fld = finalTy.GetField(templateCacheField.Field.Name)
            fld.SetValue(null, instance)
        finalTy

and ReaderTemplate<'ent>() =
    static let badNamePartRegex = System.Text.RegularExpressions.Regex(@"[^a-zA-Z0-9_.]+")
    static let entType = typeof<'ent>
    static let buildTemplate (mappings : UserTypeLibrary) =
        let moduleBuilder =
            let assembly = AssemblyName("RuntimeReaders." + badNamePartRegex.Replace(entType.FullName, "_"))
            let assemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(assembly, AssemblyBuilderAccess.Run)
            assemblyBuilder.DefineDynamicModule(assembly.Name)
        let readerBaseType = typedefof<_ EntityReader>.MakeGenericType(entType)
        let readerType =
            let builder =
                moduleBuilder.DefineType
                    ( entType.Name + "Reader"
                    , TypeAttributes.Public ||| TypeAttributes.AutoClass ||| TypeAttributes.AnsiClass
                    , readerBaseType
                    )
            StaticEntityReaderTemplate.ImplementReader(Blueprint.ofType mappings entType, builder, mappings)
        let templateType =
            let builder =
                moduleBuilder.DefineType
                    ( entType.Name + "Template"
                    , TypeAttributes.Public ||| TypeAttributes.AutoClass ||| TypeAttributes.AnsiClass
                    , typedefof<_ EntityReaderTemplate>.MakeGenericType(entType)
                    )
            ignore <| builder.DefineDefaultConstructor(MethodAttributes.Public)
            let meth =
                builder.DefineMethod
                    ( "CreateReader"
                    , MethodAttributes.Public ||| MethodAttributes.Virtual
                    , readerBaseType
                    , Type.EmptyTypes
                    )
            (Stack.empty, IL(meth.GetILGenerator())) |-->
                cil {
                    yield newobj0 (readerType.GetConstructor(Type.EmptyTypes))
                    yield ret
                } |> ignore
            builder.CreateTypeInfo().AsType()
        Activator.CreateInstance(templateType)
        |> Unchecked.unbox : 'ent EntityReaderTemplate
    static let templatesByMapping = Dictionary()
    static let getTemplateByMapping (mappings : UserTypeLibrary) =
        // TODO concurrent dict
        lock templatesByMapping <| fun () ->
            let succ, existing = templatesByMapping.TryGetValue(mappings.Identity)
            if succ then existing else
            let generated = buildTemplate mappings
            templatesByMapping.[mappings.Identity] <- generated
            generated
    static let defaultTemplate = lazy getTemplateByMapping UserTypeLibrary.Empty
    static member Template(mappings : UserTypeLibrary) : EntityReaderTemplate<'ent> =
        getTemplateByMapping mappings
    static member Template() : EntityReaderTemplate<'ent> = defaultTemplate.Value

and InternalReaderTemplateCacheForEnt<'generatedType, 'ent>() =
    static let mutable mapping : UserTypeLibrary = Unchecked.defaultof<_>
    static let template = lazy ReaderTemplate<'ent>.Template(mapping)
    static member GetTemplate(mappings : UserTypeLibrary) =
        mapping <- mappings // it will be the same every time
        template.Value
/// Stored in static field on each generated type root. Enables stuff generated by ColumnGenerators to
/// call in and get subtype reader templates *aware* of our mapping context, for *any* element type,
/// without having to pass the mapping context around and do dict lookups on it every time.
/// Note that we can't JUST store a single static ReaderTemplate<'a> for
/// the top level entity type we're generating for. Column-generators will need to construct ReaderTemplates
/// for column types or collection-element types, not just the top-level type. So we need a fast way
/// for them to get mapping-aware readers for arbitrary types.
and InternalReaderTemplateCache<'generatedType>(mappings : UserTypeLibrary) =
    member __.Template<'ent>() = InternalReaderTemplateCacheForEnt<'generatedType, 'ent>.GetTemplate(mappings)