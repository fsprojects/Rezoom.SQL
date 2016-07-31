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
type private EntityReaderColumnGenerator(builder : TypeBuilder, column : Column) =
    abstract member DefineConstructor : unit -> Op<E THIS, E THIS>
    abstract member DefineProcessColumns :  unit -> Op<E THIS, E THIS>
    abstract member DefineImpartKnowledgeToNext : unit -> Op<E THIS, E THIS>
    abstract member DefineRead : skipOnes : Label<E THIS> -> Op<E THIS, E THIS>
    abstract member DefinePush : unit -> Op<'x, 'x S>

type private PrimitiveColumnGenerator(builder, column, primitive : Primitive) =
    inherit EntityReaderColumnGenerator(builder, column)
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

type private StaticEntityReaderTemplate<'ent>() =
    static member Template() = failwith "not implemented" : EntityReaderTemplate<'ent>

type private CompositeColumnGenerator(builder, column, composite : Composite) =
    inherit EntityReaderColumnGenerator(builder, column)
    let output = column.Blueprint.Value.Output
    let staticTemplate = typedefof<_ StaticEntityReaderTemplate>.MakeGenericType(output)
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

type private ManyColumnGenerator
    ( builder
    , column
    , element : ElementBlueprint
    , elementId : Column
    , conversion : ConversionMethod
    ) =
    inherit EntityReaderColumnGenerator(builder, column)
    let elemTy = element.Output
    let staticTemplate = typedefof<_ StaticEntityReaderTemplate>.MakeGenericType(elemTy)
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
        idInfo <- builder.DefineField("_m_i_" + column.Name, typeof<ColumnInfo>, FieldAttributes.Private)
        entDict <- builder.DefineField("_m_d_" + column.Name, dictTy, FieldAttributes.Private)
        refReader <- builder.DefineField("_m_r_" + column.Name, elemReaderTy, FieldAttributes.Private)
        zero // don't initialize dictionary yet
    override __.DefineProcessColumns() =
        cil {
            yield ldarg 1 // column map
            yield ldstr column.Name
            yield call2 ColumnMap.SubMapMethod
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
