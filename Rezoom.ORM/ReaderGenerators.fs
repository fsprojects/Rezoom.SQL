namespace Rezoom.ORM
open LicenseToCIL
open LicenseToCIL.Stack
open LicenseToCIL.Ops
open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit

type 'x THIS = 'x S
type 'x ENT = 'x S

[<AbstractClass>]
type private EntityReaderColumnGenerator(builder : TypeBuilder) =
    abstract member DefineConstructor : unit -> Op<E THIS, E THIS>
    abstract member DefineProcessColumns :  unit -> Op<E THIS, E THIS>
    abstract member DefineImpartKnowledgeToNext : unit -> Op<E THIS, E THIS>
    abstract member DefineRead : skipOnes : Label<E THIS> -> Op<E THIS, E THIS>
    abstract member DefinePush : unit -> Op<'x, 'x S>

type private PrimitiveColumnGenerator(builder, column, primitive : Primitive) =
    inherit EntityReaderColumnGenerator(builder)
    let output = column.Blueprint.Value.Output
    let mutable colValue = null
    let mutable colInfo = null
    let mutable found = null
    override __.DefineConstructor() =
        found <- builder.DefineField("_p_f_" + column.Name, typeof<bool>, FieldAttributes.Private)
        colInfo <- builder.DefineField("_p_i_" + column.Name, typeof<ColumnInfo>, FieldAttributes.Private)
        colValue <- builder.DefineField("_p_" + column.Name, output, FieldAttributes.Private)
        zero
    override __.DefineProcessColumns() =
        cil {
            yield dup
            yield ldarg 1 // column map
            yield ldstr column.Name
            yield call2 ColumnMap.ColumnMethod
            yield stfld colInfo
        }
    override __.DefineImpartKnowledgeToNext() =
        cil {
            yield ldarg 1
            yield castclass builder
            yield ldarg 0
            yield ldfld colInfo
            yield stfld colInfo
        }
    override __.DefineRead(skipOnes) =
        cil {
            let! skip = deflabel
            yield dup
            yield ldfld found
            yield brtrue's skipOnes
            yield dup
            yield ldfld colInfo
            yield ldc'i4 0
            yield blt's skip
            yield cil {
                yield ldarg 1 // row
                yield ldarg 0 // this
                yield ldfld colInfo // row, index
                yield generalize2 primitive.Converter
                yield stfld colValue
                yield ldarg 0
                yield dup
                yield ldc'i4 1
                yield stfld found
            }
            yield mark skip
        }
    override __.DefinePush() =
        cil {
            yield ldarg 0 // this
            yield ldfld colValue
        }

type private EntityReaderBuilder =
    {
        Ctor : E S * IL
        ProcessColumns : E S * IL
        ImpartKnowledge : E S * IL
        Read : E S * IL
        ToEntity : E S * IL
    }

type private StaticEntityReaderTemplate =
    static member ColumnGenerator(builder, column) =
        match column.Blueprint.Value.Cardinality with
        | One { Shape = Primitive p } ->
            PrimitiveColumnGenerator(builder, column, p) :> EntityReaderColumnGenerator
        | One { Shape = Composite c } ->
            CompositeColumnGenerator(builder, column, c) :> EntityReaderColumnGenerator
        | Many (element, conversion) ->
            ManyColumnGenerator(builder, Some column, element, conversion) :> EntityReaderColumnGenerator

    static member ImplementReader(builder : TypeBuilder, ty : Type, primitive : Primitive, readerBuilder) =
        let info = builder.DefineField("_i", typeof<ColumnInfo>, FieldAttributes.Private)
        let value = builder.DefineField("_v", ty, FieldAttributes.Private)
        readerBuilder.Ctor ||> ret'void |> ignore
        readerBuilder.ProcessColumns ||>
            cil {
                yield ldarg 0
                yield ldarg 1
                yield call1 ColumnMap.PrimaryColumnMethod
                yield stfld info
                yield ret'void
            } |> ignore
        readerBuilder.ImpartKnowledge ||>
            cil {
                yield ldarg 1
                yield castclass builder
                yield ldarg 0
                yield ldfld info
                yield stfld info
                yield ret'void
            } |> ignore
        readerBuilder.Read ||>
            cil {
                yield ldarg 0
                yield ldarg 1
                yield ldarg 0
                yield ldfld info
                yield generalize2 primitive.Converter
                yield stfld value
                yield ret'void
            } |> ignore
        readerBuilder.ToEntity ||>
            cil {
                yield ldarg 0
                yield ldfld value
                yield ret
            } |> ignore

    static member ImplementReader(builder : TypeBuilder, element : ElementBlueprint, conversion, readerBuilder) =
        let generator = ManyColumnGenerator(builder, None, element, conversion)
        readerBuilder.Ctor ||> 
            cil {
                yield ldarg 0
                yield generator.DefineConstructor()
                yield pop
                yield ret'void
            } |> ignore
        readerBuilder.ProcessColumns ||>
            cil {
                yield ldarg 0
                yield generator.DefineProcessColumns()
                yield pop
                yield ret'void
            } |> ignore
        readerBuilder.ImpartKnowledge ||>
            cil {
                yield ldarg 0
                yield generator.DefineImpartKnowledgeToNext()
                yield pop
                yield ret'void
            } |> ignore
        readerBuilder.Read ||>
            cil {
                let! lbl = deflabel
                yield ldarg 0
                yield generator.DefineRead(lbl)
                yield mark lbl
                yield pop
                yield ret'void
            } |> ignore
        readerBuilder.ToEntity ||>
            cil {
                yield generator.DefinePush()
                yield ret
            } |> ignore
            
    static member ImplementReader(builder, composite : Composite, readerBuilder) =
        let columns =
                [| for column in composite.Columns.Values ->
                    column, StaticEntityReaderTemplate.ColumnGenerator(builder, column)
                |]
        readerBuilder.Ctor ||>
            cil {
                yield ldarg 0
                for _, column in columns do
                    yield column.DefineConstructor()
                yield pop
                yield ret'void
            } |> ignore
        readerBuilder.ProcessColumns ||>
            cil {
                yield ldarg 0
                for _, column in columns do
                    yield column.DefineProcessColumns()
                yield pop
                yield ret'void
            } |> ignore
        readerBuilder.ImpartKnowledge ||>
            cil {
                yield ldarg 0
                for _, column in columns do
                    yield column.DefineImpartKnowledgeToNext()
                yield pop
                yield ret'void
            } |> ignore
        readerBuilder.Read ||>
            cil {
                let! skipOnes = deflabel
                yield ldarg 0
                let ones, others = columns |> Array.partition (fun (b, _) -> b.Blueprint.Value.IsOne)
                for _, column in ones do
                    yield column.DefineRead(skipOnes)
                yield mark skipOnes
                let! skipAll = deflabel
                for _, column in others do
                    yield column.DefineRead(skipAll)
                yield mark skipAll
                yield pop
                yield ret'void
            } |> ignore
        let constructorColumns =
            seq {
                for blue, column in columns do
                    match blue.Setter with
                    | SetConstructorParameter paramInfo ->
                        yield paramInfo.Position, column
                    | _ -> ()
            } |> Seq.sortBy fst |> Seq.map snd
        readerBuilder.ToEntity ||>
            cil {
                for column in constructorColumns do
                    yield column.DefinePush()
                    yield pretend
                yield newobj'x composite.Constructor
                for blue, column in columns do
                    match blue.Setter with
                    | SetField field ->
                        yield dup
                        yield column.DefinePush()
                        yield stfld field
                    | SetProperty prop ->
                        yield dup
                        yield column.DefinePush()
                        yield callvirt2'void (prop.GetSetMethod())
                    | _ -> ()
                yield ret
            } |> ignore
    static member ImplementReader(blueprint : Blueprint, builder : TypeBuilder) =
        let readerTy = typedefof<_ EntityReader>.MakeGenericType(blueprint.Output)
        let methodAttrs = MethodAttributes.Public ||| MethodAttributes.Virtual
        let readerBuilder =
            {
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
                ToEntity = Stack.empty, IL(builder
                    .DefineMethod("ToEntity", methodAttrs, blueprint.Output, Type.EmptyTypes).GetILGenerator())
            }
        match blueprint.Cardinality with
        | One { Shape = Primitive primitive } ->
            StaticEntityReaderTemplate.ImplementReader(builder, blueprint.Output, primitive, readerBuilder)
        | One { Shape = Composite composite } ->
            StaticEntityReaderTemplate.ImplementReader(builder, composite, readerBuilder)
        | Many (element, conversion) ->
            StaticEntityReaderTemplate.ImplementReader(builder, element, conversion, readerBuilder)
        builder.CreateType()

and ReaderTemplate<'ent>() =
    static let entType = typeof<'ent>
    static let template =
        let moduleBuilder =
            let assembly = AssemblyName("Readers." + entType.Name + "." + Guid.NewGuid().ToString("N"))
            let appDomain = Threading.Thread.GetDomain()
            let assemblyBuilder = appDomain.DefineDynamicAssembly(assembly, AssemblyBuilderAccess.Run)
            assemblyBuilder.DefineDynamicModule(assembly.Name)
        let readerBaseType = typedefof<_ EntityReader>.MakeGenericType(entType)
        let readerType =
            let builder =
                moduleBuilder.DefineType
                    ( entType.Name + "Reader"
                    , TypeAttributes.Public ||| TypeAttributes.AutoClass ||| TypeAttributes.AnsiClass
                    , readerBaseType
                    )
            StaticEntityReaderTemplate.ImplementReader(Blueprint.ofType entType, builder)
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
            (Stack.empty, IL(meth.GetILGenerator())) ||>
                cil {
                    yield newobj0 (readerType.GetConstructor(Type.EmptyTypes))
                    yield ret
                } |> ignore
            builder.CreateType()
        Activator.CreateInstance(templateType)
        |> Unchecked.unbox : 'ent EntityReaderTemplate
    static member Template() = template

and private CompositeColumnGenerator(builder, column, composite : Composite) =
    inherit EntityReaderColumnGenerator(builder)
    let output = column.Blueprint.Value.Output
    let staticTemplate = typedefof<_ ReaderTemplate>.MakeGenericType(output)
    let entTemplate = typedefof<_ EntityReaderTemplate>.MakeGenericType(output)
    let entReaderType = typedefof<_ EntityReader>.MakeGenericType(output)
    let mutable entReader = null
    override __.DefineConstructor() =
        entReader <- builder.DefineField("_c_r_" + column.Name, entReaderType, FieldAttributes.Private)
        cil {
            yield dup
            yield call0 (staticTemplate.GetMethod("Template"))
            yield call1 (entTemplate.GetMethod("CreateReader"))
            yield stfld entReader
        }
    override __.DefineProcessColumns() =
        cil {
            yield dup
            yield ldfld entReader
            yield ldarg 1 // column map
            yield ldstr column.Name
            yield call2 ColumnMap.SubMapMethod
            yield callvirt2'void (entReaderType.GetMethod("ProcessColumns"))
        }
    override __.DefineImpartKnowledgeToNext() =
        cil {
            yield ldarg 0
            yield ldfld entReader
            yield ldarg 1
            yield castclass builder
            yield ldfld entReader
            yield callvirt2'void (entReaderType.GetMethod("ImpartKnowledgeToNext"))
        }
    override __.DefineRead(skipOnes) =
        cil {
            yield dup
            yield ldfld entReader
            yield ldarg 1
            yield callvirt2'void (entReaderType.GetMethod("Read"))
        }
    override __.DefinePush() =
        cil {
            yield ldarg 0
            yield ldfld entReader
            yield callvirt1 (entReaderType.GetMethod("ToEntity"))
        }

and private ManyColumnGenerator
    ( builder
    , column : Column option
    , element : ElementBlueprint
    , conversion : ConversionMethod
    ) =
    inherit EntityReaderColumnGenerator(builder)
    let elementId =
        match element.Shape with
        | Composite { Identity = Some id } -> id
        | _ -> failwith "Unsupported collection type"
    let elemTy = element.Output
    let staticTemplate = typedefof<_ ReaderTemplate>.MakeGenericType(elemTy)
    let entTemplate = typedefof<_ EntityReaderTemplate>.MakeGenericType(elemTy)
    let elemReaderTy = typedefof<_ EntityReader>.MakeGenericType(elemTy)
    let dictTy = typedefof<Dictionary<_, _>>.MakeGenericType(elementId.Blueprint.Value.Output, elemReaderTy)
    let idTy = elementId.Blueprint.Value.Output
    let idConverter =
        match elementId.Blueprint.Value.Cardinality with
        | One { Shape = Primitive prim } ->
            prim.Converter
        | One _ -> failwith "Composite keys are not supported"
        | Many _ -> failwith "Collections as keys are not supported"
    let mutable entDict = null
    let mutable refReader = null
    let mutable idInfo = null
    override __.DefineConstructor() =
        let name = defaultArg (column |> Option.map (fun c -> c.Name)) "self"
        idInfo <- builder.DefineField("_m_i_" + name, typeof<ColumnInfo>, FieldAttributes.Private)
        entDict <- builder.DefineField("_m_d_" + name, dictTy, FieldAttributes.Private)
        refReader <- builder.DefineField("_m_r_" + name, elemReaderTy, FieldAttributes.Private)
        zero // don't initialize dictionary yet
    override __.DefineProcessColumns() =
        cil {
            yield ldarg 1 // column map
            match column with
            | Some column ->
                yield ldstr column.Name
                yield call2 ColumnMap.SubMapMethod
            | None -> ()
            let! sub = deflocal typeof<ColumnMap>
            yield stloc sub
            yield ldarg 0 // this
            yield ldloc sub
            yield ldstr elementId.Name
            yield call2 ColumnMap.ColumnMethod
            yield stfld idInfo
            yield ldloc sub
            let! skip = deflabel
            yield brfalse's skip
            yield cil {
                yield ldarg 0
                yield call0 (staticTemplate.GetMethod("Template"))
                yield call1 (entTemplate.GetMethod("CreateReader"))
                yield dup
                yield ldloc sub
                yield callvirt2'void (elemReaderTy.GetMethod("ProcessColumns"))
                yield stfld refReader
            }
            yield mark skip
        }
    override __.DefineImpartKnowledgeToNext() =
        cil {
            yield ldarg 1
            yield castclass builder
            yield ldarg 0
            yield ldfld idInfo
            yield stfld idInfo

            let! nread = deflabel
            let! exit = deflabel
            yield dup
            yield ldfld refReader
            yield brfalse's nread
            yield cil {
                yield ldarg 1
                yield ldarg 0
                yield ldfld refReader
                yield castclass builder
                yield call0 (staticTemplate.GetMethod("Template"))
                yield call1 (entTemplate.GetMethod("CreateReader"))
                let! loc = deflocal elemReaderTy
                yield dup
                yield stloc loc
                // that, oldReader, newReader
                yield callvirt2'void (elemReaderTy.GetMethod("ImpartKnowledgeToNext"))
                // that
                yield ldloc loc
                yield stfld refReader
                yield br's exit
            }
            yield mark nread
            yield cil {
                yield ldarg 1
                yield ldnull
                yield stfld refReader
            }
            yield mark exit
        }
    override __.DefineRead(_) =
        cil {
            let! skip = deflabel
            yield dup
            yield ldfld refReader
            yield brfalse skip
            yield cil {
                yield ldarg 1 // row
                yield ldarg 0 // row, this
                yield ldfld idInfo // row, colinfo
                yield ldfld (typeof<ColumnInfo>.GetField("Index")) // row, index
                yield callvirt2 (typeof<Row>.GetMethod("IsNull")) // isnull
                yield brtrue skip
                
                yield ldarg 1 // row
                yield ldarg 0 // row, this
                yield ldfld idInfo // row, colinfo
                yield generalize2 idConverter // id
                
                let! id = deflocal idTy
                yield stloc id
                let! entReader = deflocal elemReaderTy
                yield dup
                yield ldfld entDict
                yield ldloc id
                yield ldloca entReader
                yield call3 (dictTy.GetMethod("TryGetValue"))
                let! readRow = deflabel
                yield brtrue's readRow
                
                yield dup
                yield ldfld entDict
                yield ldloc id
                yield call0 (staticTemplate.GetMethod("Template"))
                yield call1 (entTemplate.GetMethod("CreateReader"))
                yield dup
                yield stloc entReader
                yield call3'void (dictTy.GetMethod("Add", [| idTy; elemReaderTy |]))
                yield dup
                yield ldfld refReader
                yield ldloc entReader
                yield callvirt2'void (elemReaderTy.GetMethod("ImpartKnowledgeToNext"))

                yield mark readRow
                yield ldloc entReader
                yield ldarg 1 // row
                yield callvirt2'void (elemReaderTy.GetMethod("Read"))
            }
            yield mark skip
        }
    override __.DefinePush() =
        cil {
            yield ldarg 0
            yield ldfld entDict
            yield call1 (dictTy.GetProperty("Values").GetGetMethod())
            yield generalize conversion
        }
