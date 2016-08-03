namespace Rezoom.ORM
open LicenseToCIL
open LicenseToCIL.Stack
open LicenseToCIL.Ops
open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit

type private CompositeColumnGenerator(builder, column, composite : Composite) =
    inherit EntityReaderColumnGenerator(builder)
    let output = column.Blueprint.Value.Output
    let staticTemplate = Generation.readerTemplateGeneric.MakeGenericType(output)
    let entTemplate = typedefof<_ EntityReaderTemplate>.MakeGenericType(output)
    let entReaderType = typedefof<_ EntityReader>.MakeGenericType(output)
    let requiresSelf = composite.ReferencesQueryParent
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
    override __.RequiresSelfReferenceToPush = requiresSelf
    override __.DefinePush(self) =
        cil {
            yield ldarg 0
            yield ldfld entReader
            if requiresSelf then
                yield dup
                yield ldloc self
                if output.IsValueType then yield box'val output
                yield callvirt2'void (entReaderType.GetMethod("SetQueryParent"))
            yield callvirt1 (entReaderType.GetMethod("ToEntity"))
        }
