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
    abstract member DefineSetQueryParent : unit -> Op<E THIS, E THIS>
    default __.DefineSetQueryParent() = zero
    abstract member RequiresSelfReferenceToPush : bool
    default __.RequiresSelfReferenceToPush = false
    abstract member DefinePush : selfReference : Local -> Op<'x, 'x S>

module private Generation =
    // We'll need to reference this type in various column generator implementations,
    // but don't want to use typedefof<_> and introduce explicit mutual recursion because
    // that would require that we put all the implementations in one file. D:
    let readerTemplateGeneric =
        Assembly.GetExecutingAssembly().GetType("Rezoom.ORM.ReaderTemplate`1")