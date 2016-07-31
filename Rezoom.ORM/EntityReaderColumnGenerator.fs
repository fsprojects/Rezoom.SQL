namespace Rezoom.ORM
open LicenseToCIL
open LicenseToCIL.Stack
open LicenseToCIL.Ops
open System
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
    let mutable colIndex = null
    let mutable found = null
    override __.DefineConstructor() =
        found <- builder.DefineField("_p_f_" + column.Name, typeof<bool>, FieldAttributes.Private)
        colIndex <- builder.DefineField("_p_i_" + column.Name, typeof<int>, FieldAttributes.Private)
        colValue <- builder.DefineField("_p_" + column.Name, output, FieldAttributes.Private)
        zero
    override __.DefineProcessColumns() =
        cil {
            yield dup
            yield ldarg 1 // column map
            yield ldstr column.Name
            yield call2 ColumnMap.ColumnMethod
            yield stfld colIndex
        }
    override __.DefineImpartKnowledgeToNext() =
        cil {
            yield ldarg 1
            yield castclass builder
            yield ldarg 0
            yield ldfld colIndex
            yield stfld colIndex
        }
    override __.DefineRead(skipOnes) =
        cil {
            let! skip = deflabel
            yield dup
            yield ldfld found
            yield brtrue's skipOnes
            yield dup
            yield ldfld colIndex
            yield ldc'i4 0
            yield blt's skip
            yield cil {
                yield ldarg 1 // row
                yield ldarg 0 // this
                yield ldfld colIndex // row, index
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