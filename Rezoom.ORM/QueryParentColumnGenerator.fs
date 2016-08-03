namespace Rezoom.ORM
open LicenseToCIL
open LicenseToCIL.Stack
open LicenseToCIL.Ops
open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit

type private QueryParentColumnGenerator(builder, column) =
    inherit EntityReaderColumnGenerator(builder)
    let output = column.Blueprint.Value.Output
    let mutable queryParent = null
    override __.DefineConstructor() =
        queryParent <- builder.DefineField("_qp" + column.Name, output, FieldAttributes.Private)
        zero
    override __.DefineProcessColumns() = zero
    override __.DefineImpartKnowledgeToNext() = zero
    override __.DefineRead(skipOnes) = zero
    override __.DefineSetQueryParent() =
        cil {
            yield ldarg 0
            yield ldarg 1 // qp
            yield stfld queryParent
        }
    override __.DefinePush(_) =
        cil {
            yield ldarg 0 // this
            yield ldfld queryParent
        }