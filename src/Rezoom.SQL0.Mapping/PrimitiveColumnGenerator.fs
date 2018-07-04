namespace Rezoom.SQL.Mapping.CodeGeneration
open Rezoom.SQL.Mapping
open LicenseToCIL
open LicenseToCIL.Stack
open LicenseToCIL.Ops
open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit

type private PrimitiveColumnGenerator(builder : TypeBuilder, column, primitive : Primitive) =
    inherit EntityReaderColumnGenerator()
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
            yield brtrue skipOnes
            yield dup
            yield ldfld colInfo
            yield ldfld ColumnInfo.IndexField
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
    override __.DefinePush(_) =
        cil {
            yield ldarg 0 // this
            yield ldfld colValue
        }