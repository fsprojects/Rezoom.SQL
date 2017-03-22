[<AutoOpen>]
module private Rezoom.SQL.Mapping.CodeGeneration.CILHelpers
open LicenseToCIL
open LicenseToCIL.Stack
open LicenseToCIL.Ops

let generalize (op : Op<E S, E S>) : Op<'x S, 'x S> =
    cil {
        yield pretend
        yield op
        yield pretend
    }

let generalize2 (op : Op<E S S, E S>) : Op<'x S S, 'x S> =
    cil {
        yield pretend
        yield op
        yield pretend
    }


