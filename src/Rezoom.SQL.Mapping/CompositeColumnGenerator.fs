namespace Rezoom.SQL.Mapping.CodeGeneration
open Rezoom.SQL.Mapping
open LicenseToCIL
open LicenseToCIL.Stack
open LicenseToCIL.Ops
open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit

type private CompositeColumnGenerator(builder : TypeBuilder, column, composite : Composite) =
    inherit EntityReaderColumnGenerator()
    let output = column.Blueprint.Value.Output
    let staticTemplate = Generation.readerTemplateGeneric.MakeGenericType(output)
    let entTemplate = typedefof<_ EntityReaderTemplate>.MakeGenericType(output)
    let entReaderType = typedefof<_ EntityReader>.MakeGenericType(output)
    let requiresSelf = composite.ReferencesQueryParent
    let mutable entReader = null
    override __.DefineConstructor() =
        entReader <- builder.DefineField("_c_r_" + column.Name, entReaderType, FieldAttributes.Private)
        zero
    override __.DefineProcessColumns() =
        cil {
            let! ncase = deflabel // if submap is null
            let! sub = tmplocal typeof<ColumnMap>
            yield ldarg 1 // column map
            yield ldstr column.Name
            yield call2 ColumnMap.SubMapMethod
            yield dup
            yield stloc sub
            yield brfalse's ncase
            yield cil {
                yield dup
                yield call0 (staticTemplate.GetMethod("Template"))
                yield callvirt1 (entTemplate.GetMethod("CreateReader"))
                yield dup
                yield ldloc sub
                yield callvirt2'void Generation.processColumnsMethod
                yield stfld entReader
            }
            yield mark ncase
        }
    override __.DefineImpartKnowledgeToNext() =
        cil {
            let! ncase = deflabel
            yield ldarg 0
            yield ldfld entReader
            yield brfalse's ncase
            yield cil {
                let! newReader = tmplocal entReaderType
                yield ldarg 1
                yield castclass builder
                yield ldarg 0
                yield ldfld entReader
                yield call0 (staticTemplate.GetMethod("Template"))
                yield callvirt1 (entTemplate.GetMethod("CreateReader"))
                yield dup
                yield stloc newReader
                yield callvirt2'void (entReaderType.GetMethod("ImpartKnowledgeToNext"))
                yield ldloc newReader
                yield stfld entReader
            }
            yield mark ncase
        }
    override __.DefineRead(_) =
        cil {
            let! ncase = deflabel
            yield dup
            yield ldfld entReader
            yield brfalse's ncase
            yield cil {
                yield dup
                yield ldfld entReader
                yield ldarg 1
                yield callvirt2'void Generation.readMethod
            }
            yield mark ncase
        }
    override __.DefineSetReverse() =
        if column.ReverseRelationship.Value |> Option.isNone then zero else
        cil {
            let! skip = deflabel
            yield ldarg 1
            yield ldc'i4 column.ColumnId
            yield bne'un's skip
            yield cil {
                yield dup
                yield ldarg 2
                yield castclass composite.Output
                yield newobj1 (typedefof<_ ObjectEntityReader>.MakeGenericType(output).GetConstructor([|output|]))
                yield stfld entReader
            }
            yield mark skip
        }
    override __.RequiresSelfReferenceToPush = requiresSelf
    override __.DefinePush(self) =
        cil {
            let! ncase = deflabel
            yield ldarg 0
            yield ldfld entReader
            yield dup
            yield brfalse's ncase
            yield cil {
                match column.ReverseRelationship.Value with
                | None -> ()
                | Some rev ->
                    yield dup
                    yield ldc'i4 rev.ColumnId
                    yield ldloc self
                    if output.IsValueType then yield box'val output
                    yield callvirt3'void Generation.setReverseMethod
                yield callvirt1 (entReaderType.GetMethod("ToEntity"))
            }
            yield mark ncase
        }
