namespace Rezoom.ORM
open System
open System.Reflection
open System.Reflection.Emit
open LicenseToCIL
open LicenseToCIL.Ops
open LicenseToCIL.Stack

type private PrimitiveColumnGenerator(builder, column : Column, primitive : Primitive) =
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
    override __.DefineNext next =
        cil {
            yield ldloc next
            yield ldarg 0 // this
            yield ldfld colIndex
            yield stfld colIndex
        }
    override __.DefineRead() =
        cil {
            let! skip = deflabel
            yield dup
            yield ldfld found
            yield brtrue's skip
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