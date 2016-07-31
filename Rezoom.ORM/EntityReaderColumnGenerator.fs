namespace Rezoom.ORM
open LicenseToCIL
open LicenseToCIL.Stack
open System
open System.Reflection
open System.Reflection.Emit

type 'x THIS = 'x S
type 'x ENT = 'x S

[<AbstractClass>]
type private EntityReaderColumnGenerator(builder : TypeBuilder, column : Column) =
    member __.TypeBuilder = builder
    member __.Column = column
    abstract member DefineConstructor : unit -> Op<E THIS, E THIS>
    abstract member DefineProcessColumns :  unit -> Op<E THIS, E THIS>
    abstract member DefineNext : LocalBuilder -> Op<E THIS, E THIS>
    abstract member DefineRead : unit -> Op<E THIS, E THIS>
    abstract member DefinePush : unit -> Op<'x, 'x S>