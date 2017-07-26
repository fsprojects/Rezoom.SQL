namespace Rezoom.SQL.Mapping.CodeGeneration
open Rezoom.SQL.Mapping
open LicenseToCIL
open LicenseToCIL.Stack
open LicenseToCIL.Ops
open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit

type private ManyColumnGenerator
    ( builder : TypeBuilder
    , column : Column option
    , element : ElementBlueprint
    , conversion : ConversionMethod
    ) =
    inherit EntityReaderColumnGenerator()
    let elemTy = element.Output
    let staticTemplate = Generation.readerTemplateGeneric.MakeGenericType(elemTy)
    let entTemplate = typedefof<_ EntityReaderTemplate>.MakeGenericType(elemTy)
    let elemReaderTy = typedefof<_ EntityReader>.MakeGenericType(elemTy)
    let listTy = typedefof<_ ResizeArray>.MakeGenericType(elemReaderTy)
    let mutable entList = null
    let mutable refReader = null
    override __.DefineConstructor() =
        let name = defaultArg (column |> Option.map (fun c -> c.Name)) "self"
        entList <- builder.DefineField("_m_l_" + name, listTy, FieldAttributes.Private)
        refReader <- builder.DefineField("_m_r_" + name, elemReaderTy, FieldAttributes.Private)
        cil {
            yield ldarg 0
            yield newobj0 (listTy.GetConstructor(Type.EmptyTypes))
            yield stfld entList
        }
    override __.DefineProcessColumns() =
        cil {
            let! skip = deflabel
            yield ldarg 1 // col map
            match column with
            | Some column ->
                yield ldstr column.Name
                yield call2 ColumnMap.SubMapMethod
            | None -> ()
            let! sub = tmplocal typeof<ColumnMap>
            yield dup
            yield stloc sub // col map
            yield brfalse's skip
            yield cil {
                yield dup // this
                yield call0 (staticTemplate.GetMethod("Template")) // this, template
                yield callvirt1 (entTemplate.GetMethod("CreateReader")) // this, reader
                yield dup // this, reader, reader
                yield ldloc sub // this, reader, reader, submap
                yield callvirt2'void Generation.processColumnsMethod // this, reader
                yield stfld refReader // _
            }
            yield mark skip
        }
    override __.DefineImpartKnowledgeToNext() =
        cil {
            yield ldarg 1 // that
            yield ldarg 0 // that, this
            yield ldfld refReader // that, oldReader
            yield call0 (staticTemplate.GetMethod("Template")) // that, oldReader, template
            yield callvirt1 (entTemplate.GetMethod("CreateReader")) // that, oldReader, newReader
            let! newReader = deflocal elemReaderTy
            yield dup
            yield stloc newReader
            // that, oldReader, newReader
            yield callvirt2'void (elemReaderTy.GetMethod("ImpartKnowledgeToNext"))
            // that
            yield ldloc newReader
            yield stfld refReader
        }
    override __.DefineRead(_) =
        cil {
            let! entReader = deflocal elemReaderTy
            yield dup
            yield ldfld refReader
            yield ldarg 0
            yield ldfld entList // refReader, list
            yield call0 (staticTemplate.GetMethod("Template"))
            yield callvirt1 (entTemplate.GetMethod("CreateReader"))
            yield dup // refReader, list, entReader, entReader
            yield stloc entReader // refReader, list, entReader
            yield call2'void (listTy.GetMethod("Add", [| elemReaderTy |]))
            // refReader
            yield ldloc entReader // refReader, entReader
            yield callvirt2'void (elemReaderTy.GetMethod("ImpartKnowledgeToNext"))
            // ()
            yield ldloc entReader // entReader
            yield ldarg 1 // row
            yield callvirt2'void Generation.readMethod // entReader.Read(row)
        }
    override __.RequiresSelfReferenceToPush = false
    override __.DefinePush(_) =
        cil {
            yield ldarg 0
            yield ldfld entList
            yield generalize conversion
        }
